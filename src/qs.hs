process []           = []

process (first:rest) = let
	smaller = filter (<= first) rest
	bigger  = filter (>  first) rest
	in
	process smaller ++ [first] ++ process bigger
