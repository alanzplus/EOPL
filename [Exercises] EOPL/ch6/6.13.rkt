; 1 removeall
letrec
  removeall(n,s,k) =
    if null?(s)
    then (k emptylist)
    else if number?(car(s))
         then if equal?(n, car(s))
              then (removeall n cdr(s) k)
    else (removeall
           n
           cdr(s) 
           proc (alist) (k cons(car(s) alist)))
    else (removeall
           n
           car(s)
           proc (alist1)
           (removeall
             n
             cdr(s)
             proc (alist2)
             (k cons(alist1, alist2))))

; 2 occurs-in?
letrec
  occurs-in?(n,s,k) =
  if null?(s)
  then (k 0)
  else if number?(car(s))
       then if equal?(n, car(s))
            then 1
            else (occurs-in? n cdr(s) k)
       else	(occurs-in?
              n
              car(s)
              proc (flag)
                if flag
                then (k 1)
                else (occurs-in? n cdr(s) k))

; 3 remfirst
letrec
  remfirst(n,s,k) = 
    letrec
      loop(s,kk) = 
        if null?(s)
        then (kk emptylist)
        else if number?(car(s))
             then if equal?(n,car(s))
                  then cdr(s)
                  else (loop
                          cdr(s)
                          proc (val)
                            (k cons(car(s) val)))
             else
               (occurs-in?
                 n
                 car(s)
                 proc (flag)
                   if flag
                   then
                     (remfirst
                       n
                       car(s)
                       proc (val)
                         (k cons(val cdr(s))))
                   else
                      (remfirst
                        n
                        cdr(s)
                        proc (val)
                         (k cons(car(s) val))))
  in (loop s k)

; 4 depth
letrec
  depth(s,k) =
    if null?(s)
    then (k 1)
    else if number?(car(s))
         then (depth cdr(s) k)
         else
          (depth
            car(s)
            proc (d1)
              (depth
                cdr(s)
                proc (d2)
                  if less?(add1(d1), d2)
                  then (depth cdr(s) k)
                  else
                    (depth
                      car(s)
                      proc (d)
                      (k add1(d)))))

; 5 depth-with-let
letrec
  depth(s,k) =
    if null?(s)
    then (k 1)
    else if number?(car(s))
         then (depth cdr(s) k)
         else
          (depth
            car(s)
            proc (dfirst)
              (depth
                cdr(s)
                proc (drest)
                  let dfirst' = add1(dfirst)
                  in if less?(dfirst', drest)
                     then (k drest)
                     else (dfirst')))

; 6 map
letrec
  map(f,1,k) =
    if null?(l)
    then (k emptylist)
    else (map
           f
           cdr(l)
           proc (alist)
           cons((f car(l) alist)))
  square(n) = n * n
  in (map square list(1,2,3,4,5) proc (val) val)

; 7
letrec
  gnlrgtn(l,n,k) =
    if null?(l)
    then (k #f)
    else if number?(car(l))
         then if greater?(car(l),n)
              then (k car(l))
              else (gnlrgtn cdr(l),n,k)
         else
          (gnlrgtn
            car(l),
            n
            proc (val)
              if number?(val)
              then (k val)
              else
              (gnlrgtn cdr(l) n k))

; 8
letrec
  every(pred, l, k) =
    if null?(l)
    then (k 1)
    else if (pred car(l))
         then (every pred cdr(l) k)
         else 0
    in (every proc (n) greater?(n,5) list(6,7,8,9) proc(val) val)
