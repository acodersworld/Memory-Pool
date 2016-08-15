#include "memory_pool.h"
#include <stdio.h>
#include <vector>
#include <assert.h>
#include <stdlib.h>
#include <time.h>

int inUse = 0;
int inFree = 0;

struct Data 
{
	int inUse;
	int inFree;
};

void WalkPoolCallback( void* pMem, size_t size, bool inUse, void* pData )
{
	Data& d = *((Data*)pData);
	printf( "chunk %p %s, size - %lu\n", pMem, inUse?"in use":"free", size);

	if( inUse )
	{
		++d.inUse;
	}
	else
	{
		++d.inFree;
	}
}

char buffer[1024*1024*8];
int main(int argc, char* argv[])
{
	MemoryPool* pPool = MemoryPool::createInPlace(buffer, sizeof(buffer));

	/* allocates from the back of the pool to help prevent fragmentation */
	MemoryPool::free(pPool->allocBack(112));
	MemoryPool::free(pPool->allocBack(1024));
	MemoryPool::free(pPool->allocBack(500));
	MemoryPool::free(pPool->allocBack(2000));
	
	const int iterations = 1000;

	srand( time(NULL) );
	for( int counter = 0; counter<iterations; ++counter )
	{
		MemoryPool* pPool = MemoryPool::createInPlace(buffer, sizeof(buffer));
		{
			std::vector<void*> a;a.reserve(10000);
			for( int i = 0;i<10000; ++i )
			{
				a.push_back(pPool->alloc(8));
			}
			for( int i = 0;i<10000; ++i )
			{
				MemoryPool::free(a[i]);
			}
		}

		std::vector<int> alloc;
		std::vector<int> frees;
		int nAllocations = 0;
		int nFree = 0;
		int nBytes = 0;
		{
			
			alloc.reserve(30000);
			frees.reserve(30000);
			for( int i = 0;i<10000; ++i )
			{
				frees.push_back( rand() );
				if( rand() % 7 || ( nAllocations <= (nFree*2) ) )
				{
					int size = 8 + (rand() % (2048));
					alloc.push_back(size);
					nBytes += size;
					++nAllocations;
				}
				else
				{
					alloc.push_back(0);
					++nFree;
				}
			}
		}

		std::vector<void*> pp;
		const int sz = alloc.size();
		pp.reserve(sz);

		{
			for(int i =0 ;i<sz; ++i )
			{
				const int size = alloc[i];
				int* p;
				if( size > 0 )
				{
					p = (int*)pPool->alloc(size);
					assert( 0 == ( (ptrdiff_t)p & 7) ); // 8 byte alignment
					assert(p);
					*p = i;
				
					pp.push_back(p);
				}
				else
				{
					const int idx = frees[i]%pp.size();
					p = (int*)pp[idx];
					MemoryPool::free(p);
					pp[idx] = pp.back();
					pp.pop_back();
				}
			}
			MemoryPool::Stats stats;
			pPool->calcStats(stats);
			printf( "Pool Status: %d %d %d %d\n", stats.nBytesFree, stats.nBytesInUse, stats.nFree, stats.nInUse );

			for( int i = 0; !pp.empty(); ++i )
			{
				const int idx = frees[i]%pp.size();
				void* p = pp[idx];
				pp[idx] = pp.back();
				pp.pop_back();
				MemoryPool::free(p);
			}
			pPool->forceConsolidate();
			pPool->calcStats(stats);
			printf( "Pool Status: %d %d %d %d\n\n", stats.nBytesFree, stats.nBytesInUse, stats.nFree, stats.nInUse );
		}
	}

	return 0;
}

