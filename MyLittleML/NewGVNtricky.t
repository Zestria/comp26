$ ls
  NewGVNtricky.ll
$ cat 1.ll
Here second simplifycfg doesn't do what expected
$ opt-18 --passes='simplifycfg,early-cse,tailcallelim,reassociate,gvn,simplifycfg' -print-after-all NewGVNtricky.ll -o /dev/null
  $ opt-18  -O2 -print-after-all NewGVNtricky.ll -o /dev/null
  ; *** IR Dump After Annotation2MetadataPass on [module] ***
  ; ModuleID = 'NewGVNtricky.ll'
  source_filename = "NewGVNtricky.ll"
  
  declare void @use1(i32)
  
  declare void @use2(i32)
  
  declare void @use3(i32)
  
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    br i1 %cond2, label %case_y_1, label %case_y_2
  
  case_y_1:                                         ; preds = %merge
    %y1 = add i32 %a, %b
    ret i32 %y1
  
  case_y_2:                                         ; preds = %merge
    %y2 = add i32 %b, %a
    ret i32 %y2
  }