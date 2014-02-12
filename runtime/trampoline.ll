%worker   = type opaque
%instance = type opaque
%match    = type { void (%worker*, %match*)*, %instance* }

define void @worker_trampoline(%worker* %w, %match* %m) nounwind alwaysinline {
  %f_ptr = getelementptr inbounds %match* %m, i32 0, i32 0
  %f     = load void (%worker*, %match*)** %f_ptr

  tail call fastcc void %f(%worker* %w, %match* %m) nounwind
  ret void
}
