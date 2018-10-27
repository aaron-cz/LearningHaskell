data Dir = South | North | West | East | Shoot
    -- !! missing from final exam
    deriving (Eq, Show)

robot :: (Int, Int) -> [Dir] -> [(Int, Int, Dir)]
robot (x, y) ds = robotHidden (x, y, North) ds

robotHidden :: (Int, Int, Dir) -> [Dir] -> [(Int, Int, Dir)]
robotHidden _       []     = []
robotHidden lastPos (d:ds) = 
    let nextPos = maniRobot lastPos d
        otherShots = robotHidden nextPos ds
    in  if (d == Shoot) then (lastPos:otherShots)
        else otherShots

maniRobot :: (Int, Int, Dir) -> Dir -> (Int, Int, Dir)
maniRobot p           Shoot = p
maniRobot (x0, y0, _) South = (x0, y0 + 1, South)
maniRobot (x0, y0, _) North = (x0, y0 - 1, North)
maniRobot (x0, y0, _) West  = (x0 - 1, y0, West)
maniRobot (x0, y0, _) East  = (x0 + 1, y0, East)

dirExample :: [Dir]
dirExample = [North, Shoot, East, Shoot, West, Shoot]