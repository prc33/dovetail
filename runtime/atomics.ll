
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
  %old = cmpxchg i32* %ptr, i32 %cmp, i32 %new seq_cst
  %success = icmp eq i32 %cmp, %old
  ret i1 %success
}

%node = type opaque

define i1 @llvm_cas_ptr(%node** %ptr, %node* %cmp, %node *%new) nounwind alwaysinline {
  %iptr = bitcast %node** %ptr to i64*
  %icmp = ptrtoint %node* %cmp to i64
  %inew = ptrtoint %node* %new to i64

  %old = cmpxchg i64* %iptr, i64 %icmp, i64 %inew seq_cst
  %success = icmp eq i64 %icmp, %old
  ret i1 %success
}

%cell = type opaque

define void @llvm_atomic_store_ptr(%cell** %ptr, %cell* %val) nounwind alwaysinline {
  store atomic %cell* %val, %cell** %ptr monotonic, align 4
  ret void
}


