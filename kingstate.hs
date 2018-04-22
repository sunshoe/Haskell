type Stack a = [a]

push :: a -> Stack a ->((), Stack a)
push a s = ((), a : s)

pop :: Stack a -> (a, Stack a)
pop (a : s) = (a, s)

king :: Stack Int -> ((), Stack Int)
king s = let (a1, s1) = pop s in
              let (a2, s2) = pop s1 in
              let (a3, s3) = pop s2 in
              if a1 + a2 + a3 == 19 then
                let ((), t1) = push a1 s3 in
                let ((), t2) = push a3 t1 in
                ((), t2)
              else let ((), t1) = push 7 s3 in
                   let ((), t2) = push 2 t1 in
                   let ((), t3) = push 10 t2 in
                   ((), t3)

pushState ::  
