/*
	The MIT License (MIT)
	Copyright (c) 2016 Benjamin Han

	Permission is hereby granted, free of charge, to any person obtaining a copy of 
	this software and associated documentation files (the "Software"), to deal in the 
	Software without restriction, including without limitation the rights to use, copy, 
	modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, 
	and to permit persons to whom the Software is furnished to do so, subject to the 
	following conditions:

	The above copyright notice and this permission notice shall be included in all 
	copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
	INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A 
	PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT 
	HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF 
	CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE 
	OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

	Benjamin Han
	acodersworld@gmail.com
*/

#pragma once
#include <stddef.h> // size_t 
//#define MEMORY_POOL_USE_CANARY

class MemoryPool
{
public:

	static MemoryPool*	createInPlace( void* pBuffer, size_t bufferSize );
	static void			destroy( MemoryPool* p );

	bool		isInPool( const void* p );
	void*		alloc( size_t size );
	void*		allocAligned( size_t size, size_t alignment ); // alignment must be power of 2, to free use freeAligned

	/* allocBack - will allocate from the back of the pool using the first viable
	block. By allocating from the back of the pool the allocation will not interfere
	with normal allocations which usually are allocated from the front of the pool.
	This allocation call is not efficient, it should be used only
	when fragmentation is a concern. It walks from the back of the pool
	to find a block. The minimum size is also clamped to the maximum small 
	block size. This is especially useful for scratch memory. */
	void*		allocBack( size_t size );
	void*		allocAlignedBack( size_t size, size_t alignment ); // alignment must be power of 2, to free use freeAligned
	static void free( void* p );
	static void freeAligned( void* aligned );

	/* returns true if there are no outstanding allocations
	WARNING: this will consolidate all small chunks */
	bool isEmpty();

	typedef void WalkPoolCallback( void* pMem, size_t size, bool inUse, void* pData );
	/* walks the entire pool and calls the callback on every chunk */
	void walkPool( WalkPoolCallback cb, void* pData ) const;

	struct Stats
	{
		int nInUse;
		int nFree;
		int nBytesInUse;
		int nBytesFree;
		int largestFreeBlockInBytes;
	};
	/* walks the entire pool to calculate the data in Stats */
	void calcStats( Stats& out ) const;

	// consolidates all small free blocks
	void forceConsolidate();

private:

	MemoryPool( size_t poolSize );
	~MemoryPool();

	typedef size_t internal_size_t;
	enum { SIZE_SZ = sizeof(internal_size_t) };

#ifdef MEMORY_POOL_USE_CANARY
	enum
	{
		CANARY_ALLOC_VALUE = 0xA770CED,
		CANARY_FREE_VALUE = 0XFF43333D
	};
#endif

	struct ChunkHeader
	{
		internal_size_t chunkSize; // including header/footer
		MemoryPool* pPool;
#ifdef MEMORY_POOL_USE_CANARY
		internal_size_t canary;
		internal_size_t padding; // alignment
#endif
	};

	struct LinkNode
	{
		LinkNode* pNext;
		LinkNode* pPrev;
	};

	struct ChunkFreeHeader : ChunkHeader
	{
		LinkNode node;
	};

	struct ChunkFooter
	{
#ifdef MEMORY_POOL_USE_CANARY
		internal_size_t canary;
#endif
		internal_size_t chunkSize; // including header/footer
	};

	enum
	{
		AllocAlignment = SIZE_SZ * 2,
		AllocAlignmentMask = AllocAlignment - 1,
		MinAllocSize = AllocAlignment + sizeof(ChunkHeader) + sizeof(ChunkFooter),
		LargestSmallBinBlockSize = 128,
		SmallBinByteSizeSpacing = AllocAlignment,
		nSmallBins = (LargestSmallBinBlockSize/SmallBinByteSizeSpacing) - (MinAllocSize/8) + 1,

		nSortedLargeBins = 2,
		nLargeBins = 32,

		/* ChunkInUseFlag 
		this flag is stored in the size of the chunk header/footer
		it signifies if the chunk is 'in use' and cannot be freed
		Large chunk are always consolidated with neighbouring 'not in use' chunks
		Small chunks are not consolidated with neighbouring chunks, they are instantly stored into their corresponding small bin list
		
		FreeSmallSizedFlag
		Because small chunks are not consolidated when free by MemoryPool::free so an additional bit is required. This is because
		when an allocation cannot be satisfied all the free small blocks are consolidated. The consolidation needs to identify
		which are chunks are not in use, but the free small chunks also have the 'in use' flag raised */
		ChunkInUseFlag = 0x1,
		FreeSmallSizedFlag = 0x2
	};

	struct Aux
	{
		static int				getSmallBinIdx( internal_size_t size );
		static int				getLargeBinIdx( internal_size_t size );
		static bool				isChunkInUse( internal_size_t chunkSize );
		static bool				isFreeChunk( internal_size_t chunkSize );
		static internal_size_t	getChunkSize( internal_size_t chunkSize );
		static void				setChunkFreeSmallSized( internal_size_t& chunkSize );
		static void				setChunkInUse( internal_size_t& chunkSize );
		static void				setChunkNotInUse( internal_size_t& chunkSize );

#ifdef MEMORY_POOL_USE_CANARY
		static void canaryInitAllocHeader( ChunkHeader* pHeader );
		static void canaryInitFreeHeader( ChunkFreeHeader* pHeader );

		static void canaryInitAllocFooter( ChunkFooter* pFooter );
		static void canaryInitFreeFooter( ChunkFooter* pFooter );

		static void checkAllocHeader( const ChunkHeader* pHeader );
		static void checkFreeHeader( const ChunkFreeHeader* pHeader );

		static void checkAllocFooter( const ChunkFooter* pFooter );
		static void checkFreeFooter( const ChunkFooter* pFooter );
#endif
		static void unlinkNode( LinkNode* pNode );
		static void linkAfterNode( LinkNode* pLinkAfterMe, LinkNode* pNode );

		static ChunkFreeHeader* getHeader( LinkNode* pNode );
	};
	
	void consolidateSmallBins();
	void consolidateSmallBin( int iBin );

	void* internalAllocChunk( ChunkFreeHeader* pHeader, internal_size_t sizeIncludingHeaderFooter );
	void* allocFromLargeBin( int iFirstBin, internal_size_t size );

	void* internalAllocChunkBack( ChunkFooter* pFooter, internal_size_t sizeIncludingHeaderFooter );
	void* getPoolBegin();
	const void* getPoolBegin() const;

	LinkNode* getSmallBin( int iBin );
	LinkNode* getLargeBin( int iBin );

	internal_size_t m_poolSize;
	LinkNode m_smallBins[nSmallBins];
	LinkNode m_largeBins[nLargeBins]; 
};
