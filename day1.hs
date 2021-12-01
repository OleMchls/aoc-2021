type Measurement = Int
type Input = [Measurement]
type SlidWindow = (Int, Int, Int)
type Pair = (Int, Int)

input :: Input
input = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

collectSlidingWindow :: Input -> Input
collectSlidingWindow x = [ x * y * z | (x,y,z) <- zip3 x (tail x) (tail (tail x)) ]

pair :: Input -> [Pair]
pair x = zip x (tail x)

increased :: Pair -> Bool
increased (x, y) = y > x

calc :: Input -> Int
calc input = length (filter increased (pair (collectSlidingWindow input)))
