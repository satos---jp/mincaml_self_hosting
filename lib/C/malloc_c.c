// gcc -c -nostdlib -m32 malloc_c.c
#include <stdint.h>

struct heap_data{
	void* fr;
	void* no;
	void* to;
};

struct heap_data heap1,heap2,*nowheap,*nextheap;

void* stack_bottom;
void* stack_top;

void	heap_init_c(void* h1,void *h1_end,void* h2,void *h2_end,void* stack_bottom_v){
	heap1.fr = heap1.no = h1;
	heap1.to = h1_end;
	heap2.fr = heap2.no = h2;
	heap2.to = h2_end;
	nowheap = &heap1;
	nextheap = &heap2;
	
	stack_bottom = stack_bottom_v;
}




int gc_num = 0;
struct heap_data gc_pos[100];

// TODO(satos) .data と .bss は分かれるっぽいのでどうにかする/どうにもしない
void register_heap_c(void* fr,void* to){
	struct heap_data d;
	d.fr = fr; d.no = (void*)0xcafebabe; d.to = to;
	gc_pos[gc_num] = d;
	gc_num += 1;
}

int malloc_type = 0;
void* NULL = 0;

void* alloc_at_heap(struct heap_data* d,int n){
	void* res = d->no;
	void* to = res + ((n+2+10) * (sizeof(void*)));
	if(to >= d->to)return NULL;
	d->no = to;
	*(int*)res = n;
	*(uint32_t*)(res+(sizeof(void*))) = (malloc_type ? 0xcadedace : 0xdeadbeef);
	res += 2 * (sizeof(void*));
	//if(res==(void*)0x08052c80)asm("int $0x3\n");
	return res;
}

void* alloc_at_heap_with_raw(struct heap_data* d,int n){
	malloc_type = 1;
	void* res = alloc_at_heap(d,n);
	malloc_type = 0;
	return res;
}


void print(const char* s,int ls){
	asm(
		"mov %1,%%edx\n" 
		"mov %0,%%ecx\n" 
		"mov $2,%%ebx\n" 
		"mov $4,%%eax\n" 
		"int $0x80\n" 
	:: "r"(s), "r"(ls));
}

void fail_c(const char* reason){
	print(reason,10);
	asm("int $0x3\n");
}


void garbage_collection_c(){
	struct heap_data d;
	d.fr = stack_top; d.no = (void*)0xcafebabe;  d.to = stack_bottom;
	gc_pos[gc_num] = d;
	
	nextheap->no = nextheap->fr;
	d.fr = nextheap->fr; d.no = (void*)0xcafebabe;  d.to = nextheap->fr;
	gc_pos[gc_num+1] = d;
	gc_num += 2;
	
	int i,k;
	void* j;
	for(i=0;i<gc_num;i++){
		//print("num\n",4);
		//if(i==3)asm("int $0x3\n");
		for(j=gc_pos[i].fr; j<gc_pos[i].to; j += sizeof(void*)){
			//print("a",1);
			void* v = *((void**)j);
			if(i == gc_num-1 && (uint32_t)v == 0xcadedace){ //rawなので飛ばす
				int dl = *((int*)(j-4));
				j += dl * 4;
				continue;
			}
			//if(j == (void*)0x80d85b4)asm("int $0x3\n");
			//if(j == (void*)0xffffcc80)asm("int $0x3\n");
			//if(j == (void*)0x804e182)asm("int $0x3\n");
			//if(j == (void*)0x804e182)print("maybe 3 times\n",14);
			//if(j == (void*)0x8050358 && v == (void*)0x08052c80)asm("int $0x3\n");
			uint32_t tag = ((uint32_t)v) & 0x70000000;
			v = (void*)(((uint32_t)v) & 0x8fffffff);
			if(tag == 0x40000000)continue; //とりあえず、intタグがついているやつは避ける。
			if(tag == 0x20000000 || tag == 0x10000000 || tag == 0x0){
				// それぞれ string, tuple, その他。
				if(v < nowheap->fr || v >= nowheap->to)continue;
				
				// vはヒープ上に載ったデータをさしてるはず。
				uint32_t d = *((uint32_t*)(v-4));
				if(d == 0xdeadbeef || d == 0xcadedace){ //未割り当て
					int dl = *((int*)(v-8));
					void* tp = NULL;
					if(d == 0xdeadbeef)tp = alloc_at_heap(nextheap,dl);
					else tp = alloc_at_heap_with_raw(nextheap,dl);
					
					if(tp == NULL)fail_c("fail_to_alloc_in_nextheap");
					
					gc_pos[gc_num-1].to = nextheap->no;
					
					//if(j == (void*)0x80d5704)asm("int $0x3\n");
					for(k=0;k<dl;k++){
						*(void**)(tp+(k*4)) = *(void**)(v+(k*4));
					}
					//if(j == (void*)0x80d5704)asm("int $0x3\n");
					*((uint32_t*)(v-4)) = 0xabcdabcd;
					*((void**)(v-8)) = tp;
					*(void**)j = (void*)((uint32_t)tp | tag);
				}
				else if(d == 0xabcdabcd){ //移行済み
					void* p = *((void**)(v-8));
					p = (void*)((uint32_t)p | tag);
					*(void**)j = p;
				}
				//else fail_c("invalid_heap_tag");
				else continue; //思ったより誤検出の可能性があるっぽいですね？
				//asm("int $0x3\n");
			}
		}
	}
	gc_num -= 2;
	
	for(j=nowheap->fr; j<nowheap->no; j += sizeof(void*)){
		*(uint32_t*)j = 0xcccccccc;
		//if(j == (void*)0x08052c80)asm("int $0x3\n");
	}
	
	print("run gc\n",7);
	//asm("int $0x3\n");
	struct heap_data *tmp = nowheap;
	nowheap = nextheap;
	nextheap = tmp;
	nextheap->no = nextheap->fr; 
}


int reentrant = 0;
int gen_string_malloc_c = 0;
void* lib_malloc_c(int n){
	asm("mov %%esp,%0\n" : "=r"(stack_top));
	//asm("int $0x3\n");
	malloc_type = gen_string_malloc_c;
	void* res = alloc_at_heap(nowheap,n);
	malloc_type = 0;
	if(res == NULL){
		if(reentrant!=0){
			fail_c("heap_exhausted");
		}
		//asm("int $0x3\n");
		garbage_collection_c();
		//asm("int $0x3\n");
		reentrant = 1;
		res = lib_malloc_c(n);
	}
	reentrant = 0;
	gen_string_malloc_c = 0;
	return res;
}

