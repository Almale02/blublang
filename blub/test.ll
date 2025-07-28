; Inside the transformed function, after saving initial context:

; Entry point (when the coroutine is first called)
entry_point:
  ; Initialize i = 0
  store i32 0, i32* %frame_i_ptr
  ; Set initial resume_target_ip to the start of the loop
  store void* @loop_body_start_label, void** %frame_resume_target_ip_ptr
  br label %loop_body_start_label ; Jump into the loop

loop_body_start_label:
  ; Load 'i' and 'arr' from the frame
  %current_i = load i32, i32* %frame_i_ptr
  %current_arr_ptr = load i8*, i8** %frame_arr_ptr

  ; Equivalent to: println("at iteration: {}", i);
  call_println_func_with_args(%current_i)

  ; --- Prepare for SUSPENSION ---
  ; Calculate the value to yield
  %array_elem = load i32, i32* getelementptr inbounds (i32, i8* %current_arr_ptr, i32 %current_i)
  ; Store the yielded value
  call store_option_i32(%frame_yielded_value_ptr, %array_elem)

  ; Increment 'i' for the *next* iteration before suspending
  %next_i = add i32 %current_i, 1
  store i32 %next_i, i32* %frame_i_ptr

  ; Set the resume_target_ip to the instruction *after* this yield point
  ; This is crucial: the jump target is the address of the conditional branch.
  store void* @after_yield_check_label, void** %frame_resume_target_ip_ptr

  ; Indicate suspension and return control to the scheduler/caller
  call void @coroutine.suspend_point() ; Compiler intrinsic or helper
  ret void ; Return to the caller

after_yield_check_label: ; <--- THIS IS THE RESUMPTION POINT
  ; Load 'i' and 'arr' again if needed (or assume they are in registers from last load)
  %current_i_after_resume = load i32, i32* %frame_i_ptr
  %current_arr_ptr_after_resume = load i8*, i8** %frame_arr_ptr

  ; (C) Point after yield (your `if` statement)
  %arr_len = call i32 @get_array_length(i8* %current_arr_ptr_after_resume)
  %last_index = sub i32 %arr_len, 1
  %is_last = icmp eq i32 %current_i_after_resume, %last_index

  br i1 %is_last, label %coroutine_return_none, label %continue_loop_or_cleanup

continue_loop_or_cleanup:
  ; No need to increment i here, it was already incremented before suspend
  ; The loop condition check is implicit in jumping back to loop_body_start_label
  ; Set resume_target_ip back to the loop body start for the next iteration
  store void* @loop_body_start_label, void** %frame_resume_target_ip_ptr
  br label %loop_body_start_label ; Continue the loop

coroutine_return_none:
  ; Store None as the final return value
  call store_option_none(%frame_yielded_value_ptr)
  ; Set resume_target_ip to indicate completion (e.g., a special "done" label)
  store void* @coroutine_done_label, void** %frame_resume_target_ip_ptr
  call void @coroutine.final_suspend()
  ret void ; Final return, indicating coroutine is finished

coroutine_done_label:
  ; This label might not be directly jumped to, but signifies the state
  ; It allows the handle to know the coroutine is done.
  unreachable
