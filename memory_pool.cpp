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

#include "memory_pool.h"
#include <new>
#include <assert.h>
#include <memory.h>

#if (__cplusplus >= 201103)
#define HAS_CPP11
#endif

//#define MEMORY_POOL_TRASH_ON_ALLOCATE_CHAR	0xeb
//#define MEMORY_POOL_TRASH_ON_FREE_CHAR		0xfc
//#define MEMORY_POOL_TRASH_INTIALIZE_CHAR	0xda

MemoryPool* MemoryPool::createInPlace( void* pBuffer, size_t bufferSize )
{
	const size_t poolSize = bufferSize - sizeof(MemoryPool);
	return new (pBuffer) MemoryPool(poolSize);
}

void MemoryPool::destroy( MemoryPool* p )
{
	p->~MemoryPool();
}

MemoryPool::MemoryPool( size_t poolSize )
{
	char* pPoolUnaligned = (char*)(this + 1);
	char* pPool = (char*)getPoolBegin();
	char* pPoolEnd = pPoolUnaligned + poolSize;
	m_poolSize = pPoolEnd - pPool;
	const internal_size_t chunkSize = m_poolSize - sizeof(ChunkHeader) - sizeof(ChunkFooter);
	assert( chunkSize > LargestSmallBinBlockSize );
	{
		ChunkFooter* pFooter = (ChunkFooter*)pPool;
		ChunkHeader* pHeader = (ChunkHeader*)(pPool + m_poolSize - sizeof(ChunkHeader));
		pHeader->pPool = this;
#ifdef MEMORY_POOL_USE_CANARY
		Aux::canaryInitAllocFooter(pFooter);
		Aux::canaryInitAllocHeader(pHeader);
#endif
		pHeader->chunkSize = ChunkInUseFlag;
		pFooter->chunkSize = ChunkInUseFlag;
	}

	memset( m_smallBins, 0, sizeof(m_smallBins) );
	memset( m_largeBins, 0, sizeof(m_largeBins) );

	{
		ChunkFreeHeader* pHeader = (ChunkFreeHeader*)((ChunkFooter*)pPool + 1);
		ChunkFooter* pFooter = (ChunkFooter*)( (char*)pHeader + chunkSize - sizeof(ChunkFooter) );
		pHeader->pPool = this;

		LinkNode* pBin = m_largeBins + Aux::getLargeBinIdx(chunkSize);
		pHeader->chunkSize = chunkSize;
		pHeader->node.pNext = NULL;
		pHeader->node.pPrev = pBin;
		pFooter->chunkSize = chunkSize;
#ifdef MEMORY_POOL_USE_CANARY
		Aux::canaryInitFreeFooter(pFooter);
		Aux::canaryInitFreeHeader(pHeader);
#endif
#ifdef MEMORY_POOL_TRASH_INTIALIZE_CHAR
		{
			char* p = (char*)(pHeader + 1);
			memset( p, MEMORY_POOL_TRASH_INTIALIZE_CHAR, (char*)pFooter - p );
		}
#endif
		pBin->pNext = &pHeader->node;
	}
}

MemoryPool::~MemoryPool()
{
}

void* MemoryPool::getPoolBegin()
{
	char* pPoolUnaligned = (char*)(this + 1);
	ChunkFooter* pPool = (ChunkFooter*)( ( (ptrdiff_t)pPoolUnaligned + AllocAlignment + AllocAlignmentMask ) & ~AllocAlignmentMask );
#ifdef HAS_CPP11
	static_assert( AllocAlignment > sizeof(ChunkFooter), "alignment is too small" ); // footer should always be smaller or equal to alignment
#else
	assert( AllocAlignment > sizeof(ChunkFooter) );
#endif
	return pPool - 1;
}

const void* MemoryPool::getPoolBegin() const
{
	return const_cast<MemoryPool*>(this)->getPoolBegin();
}

bool MemoryPool::isEmpty()
{
	consolidateSmallBins();
	const internal_size_t size = m_poolSize - sizeof(ChunkHeader) - sizeof(ChunkFooter);
	ChunkFreeHeader* pHeader = NULL;
	if( size <= LargestSmallBinBlockSize )
	{
		const int iBin = Aux::getSmallBinIdx(size);
		pHeader = Aux::getHeader( getSmallBin(iBin)->pNext );
	}
	else
	{
		const int iBin = Aux::getLargeBinIdx(size);
		pHeader = Aux::getHeader( getLargeBin(iBin)->pNext );
	}

	if( pHeader )
	{
		return ( size == Aux::getChunkSize( pHeader->chunkSize ) );
	}
	return false;
}

bool MemoryPool::isInPool( const void* p )
{
	const void* pPool = getPoolBegin();
	const void* pPoolEnd = (char*)pPool + m_poolSize - sizeof(ChunkFooter);
	return ( pPool <= p ) && ( p < pPoolEnd );
}

void* MemoryPool::alloc( size_t size )
{
	if( size < AllocAlignment )
	{
		size = AllocAlignment;
	}
	size = (size + sizeof(ChunkHeader) + sizeof(ChunkFooter) + AllocAlignmentMask) & ~AllocAlignmentMask;

	if( size <= LargestSmallBinBlockSize )
	{
		int iBin = Aux::getSmallBinIdx(size);
		for(; (iBin < nSmallBins) && !getSmallBin(iBin)->pNext; ++iBin )
			;
		if( iBin < nSmallBins )
		{
			ChunkFreeHeader* pHeader = Aux::getHeader( getSmallBin(iBin)->pNext );
			assert( pHeader );
			assert( pHeader->chunkSize >= size );
			return internalAllocChunk(pHeader, size);
		}
		return allocFromLargeBin(0, size);
	}

	const int iBin = Aux::getLargeBinIdx(size);
	return allocFromLargeBin(iBin, size);
}

/* to allocate an aligned block we allocate a block
	(size + sizeof(internal_size_t) + alignment) big
	we then offset to the correct alignment and store
	the offset before the user memory very much like
	the ChunkHeader is before the user memory in a normal
	allocation. */
void* MemoryPool::allocAligned( size_t size, size_t alignment )
{
	assert( 0 == (alignment & (alignment-1)) ); // power of 2 check
	if( alignment < AllocAlignment )
	{
		alignment = AllocAlignment;
	}

	size += sizeof(internal_size_t) + alignment;
	char* ptr = (char*)alloc( size );

	const internal_size_t mask = alignment - 1;
	char* aligned = (char*)( ( (ptrdiff_t)ptr + 1 + mask ) & ~mask );

	const internal_size_t offset = aligned - ptr;
	((internal_size_t*)aligned)[-1] = offset;
	return aligned;
}

void* MemoryPool::allocFromLargeBin( int iFirstBin, internal_size_t size )
{
	bool hasConsolidated = false;
	for(;;)
	{
		for(int iBin = iFirstBin; iBin < nLargeBins; ++iBin )
		{
			LinkNode* pBin = getLargeBin(iBin);
			ChunkFreeHeader* pCandidate = Aux::getHeader( pBin->pNext );
			while( pCandidate )
			{
				if( pCandidate->chunkSize >= size )
				{
					return internalAllocChunk(pCandidate, size);
				}
				pCandidate = Aux::getHeader( pCandidate->node.pNext );
			}
		}

		if( hasConsolidated )
		{
			break;
		}
		consolidateSmallBins();
		hasConsolidated = true;
	}
	return NULL;
}

void* MemoryPool::internalAllocChunk( ChunkFreeHeader* pHeader, internal_size_t sizeIncludingHeaderFooter )
{
#ifdef MEMORY_POOL_USE_CANARY
	{
		ChunkFooter* pFooter = (ChunkFooter*)((char*)pHeader + Aux::getChunkSize(pHeader->chunkSize) - sizeof(ChunkFooter));
		Aux::checkFreeHeader(pHeader);
		Aux::checkFreeFooter(pFooter);
	}
#endif
	
	const internal_size_t spaceLeft = Aux::getChunkSize(pHeader->chunkSize) - sizeIncludingHeaderFooter;
	LinkNode* pPrev = pHeader->node.pPrev;
	Aux::unlinkNode(&pHeader->node);
	if( spaceLeft > MinAllocSize )
	{
		// split chunks
		ChunkFooter* pFooter = (ChunkFooter*)((char*)pHeader + Aux::getChunkSize(pHeader->chunkSize) - sizeof(ChunkFooter));
		ChunkFooter* pNewFooter = (ChunkFooter*)((char*)pHeader + sizeIncludingHeaderFooter - sizeof(ChunkFooter));
		ChunkFreeHeader* pNewHeader = (ChunkFreeHeader*)(pNewFooter+1);
		pHeader->chunkSize = sizeIncludingHeaderFooter;
		pNewFooter->chunkSize = sizeIncludingHeaderFooter;
		
		pNewHeader->chunkSize = spaceLeft;
		pFooter->chunkSize = spaceLeft;
#ifdef MEMORY_POOL_USE_CANARY
		Aux::canaryInitAllocHeader(pHeader);
		Aux::canaryInitAllocFooter(pNewFooter);

		Aux::canaryInitFreeHeader(pNewHeader);
		Aux::canaryInitFreeFooter(pFooter);
#endif
		if( spaceLeft <= LargestSmallBinBlockSize )
		{
			const int iBin = Aux::getSmallBinIdx(spaceLeft);
			Aux::linkAfterNode( getSmallBin(iBin), &pNewHeader->node );
		}
		else
		{
			const int iLastBin = Aux::getLargeBinIdx(sizeIncludingHeaderFooter);
			const int iBin = Aux::getLargeBinIdx(spaceLeft);
			LinkNode* pNode = pPrev;
			if( iLastBin == iBin )
			{
				if( iBin < nSortedLargeBins )
				{
					// if pNode->pPrev == NULL, then we have reached the list head
					while( pNode->pPrev && Aux::getHeader(pNode)->chunkSize > spaceLeft )
					{
						pNode = pNode->pPrev;
					}
				}
			}
			else
			{
				pNode = getLargeBin(iBin);
				if( iBin < nSortedLargeBins )
				{
					for(;;)
					{
						ChunkFreeHeader* pNext = Aux::getHeader( pNode->pNext );
						if( pNext && (pNext->chunkSize < spaceLeft) )
						{
							pNode = &pNext->node;
						}
						else
						{
							break;
						}
					}
				}
			}

			Aux::linkAfterNode( pNode, &pNewHeader->node );
		}
	}
	else
	{
#ifdef MEMORY_POOL_USE_CANARY
		ChunkFooter* pFooter = (ChunkFooter*)((char*)pHeader + Aux::getChunkSize(pHeader->chunkSize) - sizeof(ChunkFooter));
		Aux::canaryInitAllocHeader(pHeader);
		Aux::canaryInitAllocFooter(pFooter);
#endif
	}

	internal_size_t size = Aux::getChunkSize(pHeader->chunkSize);
	ChunkFooter* pFooter = (ChunkFooter*)((char*)pHeader + size - sizeof(ChunkFooter));
	Aux::setChunkInUse( size );
	pHeader->chunkSize = size;
	pFooter->chunkSize = size;
	pHeader->pPool = this;
	void* ptr = (void*)( (ChunkHeader*)pHeader + 1);
#ifdef MEMORY_POOL_TRASH_ON_ALLOCATE_CHAR
	memset( ptr, MEMORY_POOL_TRASH_ON_ALLOCATE_CHAR, (char*)pFooter - (char*)ptr);
#endif
	return ptr;
}

void* MemoryPool::allocBack( size_t size )
{
	if( size < LargestSmallBinBlockSize )
	{
		size = LargestSmallBinBlockSize;
	}
	const internal_size_t sizeIncludingHeaderFooter = (size + sizeof(ChunkHeader) + sizeof(ChunkFooter) + AllocAlignmentMask) & ~AllocAlignmentMask;
	const void* pPoolBegin = getPoolBegin();
	const void* pPoolEnd = (char*)pPoolBegin + m_poolSize - sizeof(ChunkFooter);
	ChunkFooter* pFooter = (ChunkFooter*)( (ChunkHeader*)pPoolEnd - 1 );
	while( pFooter > pPoolBegin )
	{
		const bool isInUse = Aux::isChunkInUse(pFooter->chunkSize);
		internal_size_t chunkSize = Aux::getChunkSize(pFooter->chunkSize);
		if( !isInUse && chunkSize >= sizeIncludingHeaderFooter )
		{
			return internalAllocChunkBack( pFooter, sizeIncludingHeaderFooter );
		}
		pFooter = (ChunkFooter*)((char*)pFooter - chunkSize);
	}
	return NULL;
}

void* MemoryPool::allocAlignedBack( size_t size, size_t alignment )
{
	assert( 0 == (alignment & (alignment-1)) ); // power of 2 check
	if( alignment < AllocAlignment )
	{
		alignment = AllocAlignment;
	}

	size += sizeof(internal_size_t) + alignment;
	char* ptr = (char*)allocBack( size );

	const internal_size_t mask = alignment - 1;
	char* aligned = (char*)( ( (ptrdiff_t)ptr + 1 + mask ) & ~mask );

	const internal_size_t offset = aligned - ptr;
	((internal_size_t*)aligned)[-1] = offset;
	return aligned;
}

void* MemoryPool::internalAllocChunkBack( ChunkFooter* pFooter, internal_size_t sizeIncludingHeaderFooter )
{
	const internal_size_t chunkSize = Aux::getChunkSize(pFooter->chunkSize);
	ChunkFreeHeader* pHeader = (ChunkFreeHeader*)((char*)pFooter + sizeof(ChunkFooter) - chunkSize);
#ifdef MEMORY_POOL_USE_CANARY
	Aux::checkFreeHeader(pHeader);
	Aux::checkFreeFooter(pFooter);
#endif
	LinkNode* pPrev = pHeader->node.pPrev;
	Aux::unlinkNode(&pHeader->node);
	const internal_size_t spaceLeft = chunkSize - sizeIncludingHeaderFooter;
	if( spaceLeft < LargestSmallBinBlockSize )
	{
		/* only split if the split block is larger than largest small block size,
		small blocks hang around much longer than large blocks so it would defeat the
		purpose of this function */
		internal_size_t chunkSizeInUse = chunkSize;
		Aux::setChunkInUse( chunkSizeInUse );
		pHeader->chunkSize = chunkSizeInUse;
		pFooter->chunkSize = chunkSizeInUse;
		pHeader->pPool = this;
#ifdef MEMORY_POOL_USE_CANARY
		Aux::canaryInitAllocHeader(pHeader);
		Aux::canaryInitAllocFooter(pFooter);
#endif
		void* ptr = (ChunkHeader*)pHeader + 1;
#ifdef MEMORY_POOL_TRASH_ON_ALLOCATE_CHAR
		memset( ptr, MEMORY_POOL_TRASH_ON_ALLOCATE_CHAR, (char*)pFooter - (char*)ptr);
#endif
		return ptr;
	}
	else
	{
		ChunkFreeHeader* pNewHeader = (ChunkFreeHeader*)((char*)pFooter - sizeIncludingHeaderFooter + sizeof(ChunkFooter) );
		ChunkFooter* pNewFooter = (ChunkFooter*)((char*)pNewHeader - sizeof(ChunkFooter));
		pHeader->chunkSize = spaceLeft;
		pNewFooter->chunkSize = spaceLeft;
		pHeader->pPool = this;
	
		{
			internal_size_t chunkSizeInUse = sizeIncludingHeaderFooter;
			Aux::setChunkInUse( chunkSizeInUse );
			pNewHeader->chunkSize = chunkSizeInUse;
			pFooter->chunkSize = chunkSizeInUse;
			pNewHeader->pPool = this;
		}

#ifdef MEMORY_POOL_USE_CANARY
		Aux::canaryInitFreeHeader(pHeader);
		Aux::canaryInitFreeFooter(pNewFooter);

		Aux::canaryInitAllocHeader(pNewHeader);
		Aux::canaryInitAllocFooter(pFooter);
#endif

		const int iLastBin = Aux::getLargeBinIdx(sizeIncludingHeaderFooter);
		const int iBin = Aux::getLargeBinIdx(spaceLeft);
		LinkNode* pNode = pPrev;
		if( iLastBin == iBin )
		{
			if( iBin < nSortedLargeBins )
			{
				while( pNode->pPrev && Aux::getHeader(pNode)->chunkSize > spaceLeft )
				{
					pNode = pNode->pPrev;
				}
			}
		}
		else
		{
			pNode = getLargeBin(iBin);
			if( iBin < nSortedLargeBins )
			{
				for(;;)
				{
					ChunkFreeHeader* pNext = Aux::getHeader(pNode->pNext);
					if( pNext && (pNext->chunkSize < spaceLeft) )
					{
						pNode = &pNext->node;
					}
					else
					{
						break;
					}
				}
			}
		}

		Aux::linkAfterNode( pNode, &pHeader->node );

		void* ptr = (ChunkHeader*)pNewHeader + 1;
#ifdef MEMORY_POOL_TRASH_ON_ALLOCATE_CHAR
		memset( ptr, MEMORY_POOL_TRASH_ON_ALLOCATE_CHAR, (char*)pFooter - (char*)ptr);
#endif
		return ptr;
	}
}

MemoryPool::LinkNode* MemoryPool::getSmallBin( int iBin )
{
	assert( iBin >= 0 );
	assert( iBin < nSmallBins );
	return m_smallBins + iBin;
}

MemoryPool::LinkNode* MemoryPool::getLargeBin( int iBin )
{
	assert( iBin >= 0 );
	assert( iBin < nLargeBins );
	return m_largeBins + iBin;
}

/* taken from dlmalloc :) */
int MemoryPool::Aux::getLargeBinIdx(internal_size_t sz)
{
	unsigned int  x = sz / LargestSmallBinBlockSize; 
	if( x >= (1<<(nLargeBins>>2)) )
	{
		return nLargeBins-1;
	}
	unsigned int m;            /* bit position of highest set bit of m */

    /*
      Based on branch-free nlz algorithm in chapter 5 of Henry
      S. Warren Jr's book "Hacker's Delight".
    */

    unsigned int n = ((x - 0x100) >> 16) & 8;
    x <<= n; 
    m = ((x - 0x1000) >> 16) & 4;
    n += m; 
    x <<= m; 
    m = ((x - 0x4000) >> 16) & 2;
    n += m; 
    x = (x << m) >> 14;
    m = 13 - n + (x & ~(x>>1));

	return (m << 2) + ((sz >> (m + 6)) & 3);
}

int MemoryPool::Aux::getSmallBinIdx( internal_size_t size )
{
	return (size / SmallBinByteSizeSpacing) - (MinAllocSize/8);
}

bool MemoryPool::Aux::isChunkInUse( internal_size_t chunkSize )
{
	return (chunkSize & ChunkInUseFlag) ? true : false;
}

bool MemoryPool::Aux::isFreeChunk( internal_size_t chunkSize )
{
	return 0 != ( (chunkSize & FreeSmallSizedFlag) | (~chunkSize & ChunkInUseFlag) );
}

MemoryPool::internal_size_t MemoryPool::Aux::getChunkSize( internal_size_t chunkSize )
{
	return chunkSize & ~(ChunkInUseFlag|FreeSmallSizedFlag);
}

void MemoryPool::Aux::setChunkFreeSmallSized( internal_size_t& chunkSize )
{
	chunkSize |= FreeSmallSizedFlag;
}

void MemoryPool::Aux::setChunkInUse( internal_size_t& chunkSize )
{
	chunkSize |= ChunkInUseFlag;
}

void MemoryPool::Aux::setChunkNotInUse( internal_size_t& chunkSize )
{
	chunkSize &= ~ChunkInUseFlag;
}

void MemoryPool::Aux::unlinkNode( LinkNode* pNode )
{
	LinkNode* pNext = pNode->pNext;
	LinkNode* pPrev = pNode->pPrev;

	pPrev->pNext = pNext;
	if( pNext )
	{
		pNext->pPrev = pPrev;
	}
}

void MemoryPool::Aux::linkAfterNode( LinkNode* pLinkAfterMe, LinkNode* pNode )
{
	LinkNode* pPrev = pLinkAfterMe;
	LinkNode* pNext = pLinkAfterMe->pNext;

	pNode->pPrev = pPrev;
	pNode->pNext = pNext;

	pPrev->pNext = pNode;
	if( pNext )
	{
		pNext->pPrev = pNode;
	}
}

MemoryPool::ChunkFreeHeader* MemoryPool::Aux::getHeader( LinkNode* pNode )
{
	if( pNode )
	{
		const ptrdiff_t offset = (ptrdiff_t)&((ChunkFreeHeader*)0)->node;
		return (ChunkFreeHeader*)( (char*)pNode - offset );
	}
	return NULL;
}

void MemoryPool::free( void* p )
{
	if( !p )
	{
		return;
	}

	ChunkFreeHeader* pHeader = (ChunkFreeHeader*)((char*)p - sizeof(ChunkHeader));
	internal_size_t size = Aux::getChunkSize( pHeader->chunkSize );
	ChunkFooter* pFooter = (ChunkFooter*)((char*)pHeader + size - sizeof(ChunkFooter));
	
#ifdef MEMORY_POOL_USE_CANARY
	Aux::checkAllocHeader(pHeader);
	Aux::checkAllocFooter(pFooter);
#endif
	MemoryPool* pPool = pHeader->pPool;
	
#ifdef MEMORY_POOL_TRASH_ON_FREE_CHAR
	memset( pHeader, MEMORY_POOL_TRASH_ON_FREE_CHAR, (char*)(pFooter + 1) - (char*)pHeader );
#endif

	// only consolidate large blocks
	if( size > LargestSmallBinBlockSize )
	{
		{
			ChunkFooter* pPrevFooter = ((ChunkFooter*)pHeader) - 1;
			if( !Aux::isChunkInUse(pPrevFooter->chunkSize) )
			{
				const internal_size_t sz = Aux::getChunkSize(pPrevFooter->chunkSize);
				ChunkFreeHeader* pPrevHeader = (ChunkFreeHeader*)(((char*)pHeader) - sz);
				size += sz;
#ifdef MEMORY_POOL_USE_CANARY
				Aux::checkFreeHeader(pPrevHeader);
				Aux::checkFreeFooter(pPrevFooter);
#endif
				Aux::unlinkNode(&pPrevHeader->node);
				pHeader = pPrevHeader;
			}
		}

		{
			ChunkFreeHeader* pNextHeader = (ChunkFreeHeader*)(((ChunkFooter*)pFooter) + 1);
			if( !Aux::isChunkInUse(pNextHeader->chunkSize) )
			{
				ChunkFooter* pNextFooter = (ChunkFooter*)((char*)pNextHeader + pNextHeader->chunkSize - sizeof(ChunkFooter));
				const internal_size_t sz = Aux::getChunkSize(pNextHeader->chunkSize);
				size += sz;
#ifdef MEMORY_POOL_USE_CANARY
				Aux::checkFreeHeader(pNextHeader);
				Aux::checkFreeFooter(pNextFooter);
#endif
				Aux::unlinkNode(&pNextHeader->node);
				pFooter = pNextFooter;
			}
		}

#ifdef MEMORY_POOL_USE_CANARY
		Aux::canaryInitFreeHeader(pHeader);
		Aux::canaryInitFreeFooter(pFooter);
#endif
		pHeader->chunkSize = size;
		pFooter->chunkSize = size;

		const int iBin = Aux::getLargeBinIdx(size);
		LinkNode* pList = pPool->getLargeBin(iBin);
		if( iBin < nSortedLargeBins )
		{
			for(;;)
			{
				ChunkFreeHeader* pNext = Aux::getHeader( pList->pNext );
				if( pNext && (pNext->chunkSize < size) )
				{
					pList = &pNext->node;
				}
				else
				{
					break;
				}
			}
		}
		Aux::linkAfterNode( pList, &pHeader->node );
	}
	else
	{
#ifdef MEMORY_POOL_USE_CANARY
		Aux::canaryInitFreeHeader(pHeader);
		Aux::canaryInitFreeFooter(pFooter);
#endif
		const int iBin = Aux::getSmallBinIdx(size);
		Aux::setChunkFreeSmallSized(size);
		Aux::setChunkInUse(size);
		pHeader->chunkSize = size;
		pFooter->chunkSize = size;

		Aux::linkAfterNode( pPool->getSmallBin(iBin), &pHeader->node );
	}
}

void MemoryPool::freeAligned( void* aligned )
{
	if( !aligned )
	{
		return;
	}
	const internal_size_t offset = ((internal_size_t*)aligned)[-1];
	char* p = (char*)aligned - offset;
	free( p );
}

void MemoryPool::consolidateSmallBins()
{
	LinkNode* pBins = m_smallBins;
	for( int i = 0; i<nSmallBins; ++i )
	{
		if( pBins[i].pNext )
		{
			consolidateSmallBin(i);
		}
	}
}

void MemoryPool::consolidateSmallBin( int iBin )
{
	LinkNode* pBin = getSmallBin(iBin);
	LinkNode tmpBin;
	memset( &tmpBin, 0, sizeof(tmpBin) );
	tmpBin.pNext = pBin->pNext;
	pBin->pNext->pPrev = &tmpBin;
	pBin->pNext = NULL;

	const internal_size_t smallSize = Aux::getChunkSize( Aux::getHeader(tmpBin.pNext)->chunkSize );
	while( tmpBin.pNext )
	{
		ChunkFreeHeader* pHeader = (ChunkFreeHeader*)Aux::getHeader( tmpBin.pNext );
		internal_size_t size = Aux::getChunkSize( pHeader->chunkSize );
		ChunkFooter* pFooter = (ChunkFooter*)((char*)pHeader + smallSize - sizeof(ChunkFooter));
#ifdef MEMORY_POOL_USE_CANARY
		Aux::checkFreeHeader(pHeader);
		Aux::checkFreeFooter(pFooter);
#endif
		Aux::unlinkNode( &pHeader->node );
		{
			/* scan from the header until we find an allocated chunk */
			ChunkFooter* pPrevFooter = ((ChunkFooter*)pHeader) - 1;
			while( Aux::isFreeChunk( pPrevFooter->chunkSize ) )
			{
				const internal_size_t sz = Aux::getChunkSize(pPrevFooter->chunkSize);
				ChunkFreeHeader* pPrevHeader = (ChunkFreeHeader*)(((char*)pHeader) - sz);
				size += sz;
#ifdef MEMORY_POOL_USE_CANARY
				Aux::checkFreeHeader(pPrevHeader);
				Aux::checkFreeFooter(pPrevFooter);
#endif
				Aux::unlinkNode(&pPrevHeader->node);
				pHeader = pPrevHeader;
				pPrevFooter = ((ChunkFooter*)pHeader) - 1;
			}
		}

		{
			/* scan from the footer until we find an allocated chunk */
			ChunkFreeHeader* pNextHeader = (ChunkFreeHeader*)(((ChunkFooter*)pFooter) + 1);
			while( Aux::isFreeChunk( pNextHeader->chunkSize ) )
			{
				ChunkFooter* pNextFooter = (ChunkFooter*)((char*)pNextHeader + Aux::getChunkSize(pNextHeader->chunkSize) - sizeof(ChunkFooter));
				const internal_size_t sz = Aux::getChunkSize(pNextHeader->chunkSize);
				size += sz;
#ifdef MEMORY_POOL_USE_CANARY
				Aux::checkFreeHeader(pNextHeader);
				Aux::checkFreeFooter(pNextFooter);
#endif
				Aux::unlinkNode(&pNextHeader->node);
				pFooter = pNextFooter;
				pNextHeader = (ChunkFreeHeader*)(((ChunkFooter*)pFooter) + 1);
			}
		}

		if( size > LargestSmallBinBlockSize )
		{
#ifdef MEMORY_POOL_USE_CANARY
			Aux::canaryInitFreeHeader(pHeader);
			Aux::canaryInitFreeFooter(pFooter);
#endif
			pHeader->chunkSize = size;
			pFooter->chunkSize = size;

			const int iBin = Aux::getLargeBinIdx(size);
			LinkNode* pList = getLargeBin(iBin);
			if( iBin < nSortedLargeBins )
			{
				for(;;)
				{
					LinkNode* pNext = pList->pNext;
					if( pNext && (Aux::getHeader(pNext)->chunkSize < size) )
					{
						pList = pList->pNext;
					}
					else
					{
						break;
					}
				}
			}
			Aux::linkAfterNode( pList, &pHeader->node );
		}
		else
		{
#ifdef MEMORY_POOL_USE_CANARY
			Aux::canaryInitFreeHeader(pHeader);
			Aux::canaryInitFreeFooter(pFooter);
#endif
			const int iBin = Aux::getSmallBinIdx(size);
			Aux::setChunkFreeSmallSized(size);
			Aux::setChunkInUse(size);
			pHeader->chunkSize = size;
			pFooter->chunkSize = size;

			Aux::linkAfterNode( getSmallBin(iBin), &pHeader->node );
		}
	}
}

void MemoryPool::walkPool( WalkPoolCallback cb, void* pData ) const
{
	const void* pPool = getPoolBegin();
	const void* pPoolEnd = (char*)pPool + m_poolSize - sizeof(ChunkFooter);
	const ChunkHeader* pHeader = (ChunkHeader*)((ChunkFooter*)pPool + 1);
	while( pHeader < pPoolEnd )
	{
		const bool isInUse = Aux::isChunkInUse(pHeader->chunkSize);
		const int size = Aux::getChunkSize(pHeader->chunkSize);
		cb( (void*)(pHeader + 1), size, isInUse, pData );
		pHeader = (ChunkHeader*)((char*)pHeader + size);
	}
}

void MemoryPool::calcStats( Stats& out ) const
{
	memset( &out, 0, sizeof(Stats) );

	const void* pPool = getPoolBegin();
	const void* pPoolEnd = (const char*)pPool + m_poolSize - sizeof(ChunkHeader);
	const ChunkHeader* pHeader = (const ChunkHeader*)((const ChunkFooter*)pPool + 1);
	while( pHeader < pPoolEnd )
	{
		const int size = Aux::getChunkSize(pHeader->chunkSize);
		const bool isInUse = Aux::isChunkInUse(pHeader->chunkSize);
		if( isInUse )
		{
			++out.nInUse;
			out.nBytesInUse += size;
		}
		else
		{
			++out.nFree;
			out.nBytesFree += size;

			if( out.largestFreeBlockInBytes < size )
			{
				out.largestFreeBlockInBytes = size;
			}
		}
		pHeader = (const ChunkHeader*)((char*)pHeader + size);
	}
}

void MemoryPool::forceConsolidate()
{
	consolidateSmallBins();
}

#ifdef MEMORY_POOL_USE_CANARY
void MemoryPool::Aux::canaryInitAllocHeader( ChunkHeader* pHeader )
{
	pHeader->canary = CANARY_ALLOC_VALUE;
}

void MemoryPool::Aux::canaryInitFreeHeader( ChunkFreeHeader* pHeader )
{
	pHeader->canary = CANARY_FREE_VALUE;
}

void MemoryPool::Aux::canaryInitAllocFooter( ChunkFooter* pFooter )
{
	pFooter->canary = CANARY_ALLOC_VALUE;
}

void MemoryPool::Aux::canaryInitFreeFooter( ChunkFooter* pFooter )
{
	pFooter->canary = CANARY_FREE_VALUE;
}

void MemoryPool::Aux::checkAllocHeader( const ChunkHeader* pHeader )
{
	assert( CANARY_ALLOC_VALUE == pHeader->canary );
}

void MemoryPool::Aux::checkFreeHeader( const ChunkFreeHeader* pHeader )
{
	assert( CANARY_FREE_VALUE == pHeader->canary );
}

void MemoryPool::Aux::checkAllocFooter( const ChunkFooter* pFooter )
{
	assert( CANARY_ALLOC_VALUE == pFooter->canary );
}

void MemoryPool::Aux::checkFreeFooter( const ChunkFooter* pFooter )
{
	assert( CANARY_FREE_VALUE == pFooter->canary );
}
#endif
