process []           = []
process (first:rest) = process smaller ++ [first] ++ process bigger
	where
	smaller = filter (<= first) rest
	bigger  = filter (>  first) rest