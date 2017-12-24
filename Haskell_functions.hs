delete 0 (x:xs) = x:xs
delete _ [] = []
delete k (x:xs) = delete (k-1) xs
keepl n xs = case delete (n-1) xs of
             (x:xs) -> x : keepl n xs
			 [] -> []
			 
			 
			 
len[] = 0
len (x:xs) = 1 + len xs
rem_even [[]] = []
rem_even (x:xs) = if mod(len(x))2 == 0 then rem_even(xs) else x:rem_even xs 
sublist [] = [[]]
sublist (x:xs) = [x:sublist | sublist <- sublist xs] ++ sublist xs
osublist (x:xs) = rem_even(sublist (x:xs))
			
 
replic1 n x
    | n <= 0    = []   
    | otherwise = x:replic1 (n-1) x
replic [] = [] 
replic (x:xs) = if x<=0 then error "Incorrect input!" else map (replic1 x) [x] ++ replic xs