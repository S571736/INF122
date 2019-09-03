-- A 
-- 3.1
-- types of following values
forste = ['a', 'b', 'c'] -- [Char]
andre  = ('a', 'b', 'c') -- (Char, Char, Char)
tredje = [(False, '0'), (True, '1')] -- [(Bool, Char)]
fjerde = ([False, True], ['0', '1']) -- ([Bool], [Char])
femte = [tail, tail, reverse] -- [[a] -> [a]]

-- 3.2
bools :: [Bool]
bools [b] = [b]