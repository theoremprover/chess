import Control.Parallel

process []           = []

process (first:rest) = let
	smaller = filter (<= first) rest
	bigger  = filter (>  first) rest
	in
	process smaller ++ [first] ++ process bigger

process_par _ []           = []
process_par n (first:rest) = (if n<=3 then bigger `par` smaller else smaller) ++ [first] ++ bigger
	where
	smaller = process_par (n+1) $ filter (<= first) rest
	bigger  = process_par (n+1) $ filter (>  first) rest

main = print $ process_par 0 [10000,9999..1]
