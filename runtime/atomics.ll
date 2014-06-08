
define i32 @llvm_atomic_load(i32* %ptr) nounwind readonly alwaysinline {
  %val = load atomic i32* %ptr monotonic, align 4
  ret i32 %val
}

define i32 @llvm_atomic_load_acquire(i32* %ptr) nounwind readonly alwaysinline {
  %val = load atomic i32* %ptr acquire, align 4
  ret i32 %val
}

define void @llvm_atomic_store(i32* %ptr, i32 %val) nounwind alwaysinline {
  store atomic i32 %val, i32* %ptr monotonic, align 4
  ret void
}

define void @llvm_atomic_store_release(i32* %ptr, i32 %val) nounwind alwaysinline {
  store atomic i32 %val, i32* %ptr release, align 4
  ret void
}

define void @llvm_atomic_fence() nounwind alwaysinline {
  fence seq_cst
  ret void
}

define i1 @llvm_cas(i32* %ptr, i32 %cmp, i32 %new) nounwind alwaysinline {
  %old = cmpxchg volatile i32* %ptr, i32 %cmp, i32 %new seq_cst
  %success = icmp eq i32 %cmp, %old
  ret i1 %success
}

%node = type opaque

define i1 @llvm_cas_ptr(%node** %ptr, %node* %cmp, %node *%new) nounwind alwaysinline {
  %iptr = bitcast %node** %ptr to i64*
  %icmp = ptrtoint %node* %cmp to i64
  %inew = ptrtoint %node* %new to i64

  %old = cmpxchg volatile i64* %iptr, i64 %icmp, i64 %inew seq_cst
  %success = icmp eq i64 %icmp, %old
  ret i1 %success
}

%cell = type opaque

define void @llvm_atomic_store_ptr(%cell** %ptr, %cell* %val) nounwind alwaysinline {
  store atomic %cell* %val, %cell** %ptr monotonic, align 8
  ret void
}

define void @llvm_atomic_store_seqcst(i32* %ptr, i32 %val) nounwind alwaysinline {
  store atomic volatile i32 %val, i32* %ptr seq_cst, align 4
  ret void
}

define i32 @llvm_atomic_load_seqcst(i32* %ptr) nounwind readonly alwaysinline {
  %val = load atomic volatile i32* %ptr seq_cst, align 4
  ret i32 %val
}


define %node* @llvm_atomic_load_ptr_seqcst(%node** %ptr) nounwind readonly alwaysinline {
  %iptr = bitcast %node** %ptr to i64*
  %ival = load atomic volatile i64* %iptr seq_cst, align 8
  %val = inttoptr i64 %ival to %node*
  ret %node* %val
}


define void @llvm_atomic_store_ptr_seqcst(%node** %ptr, %node* %val) nounwind alwaysinline {
  %iptr = bitcast %node** %ptr to i64*
  %ival = ptrtoint %node* %val to i64
  store atomic volatile i64 %ival, i64* %iptr seq_cst, align 8
  ret void
}


