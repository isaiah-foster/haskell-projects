
data Currency = USD Double | INR Double
    deriving Show

instance Num Currency where
    (USD x) + (USD y) = USD (x+y)
    (INR x) + (INR y) = INR (x+y)
    _+_ = error "don't understand"

    (*) = error "not supported"

    abs = error "not supported"

    signum = error "not supported"

    fromInteger = error "not supported"

    negate = error "not supported"
