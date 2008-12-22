As I wrote about in my last post, I've been traveling around the
country doing interviews for college recruiting season. It's fun to
check out a bunch of different schools, but the process itself can be
soul-sucking. There's no time to learn more than than the candidates
name and ask them a couple of meaningless questions. Granted it's a
great way to judge if they are fit for the job, but as the Dans would
say, "you're not going deep with them." So I thought I'd write a couple posts that
go deep on the shallowest of questions, Explore the space.

Since it's the day after Halloween and I'm still hungover from our
party last night, let's do the Spoooooky Maze problem.

"Today we will take you into a dazzling world of Fantasy and
 Adventure! The world of Labyrinth!"

So any way in this problem, you're Jennifer Connolly and you're trying
to get to the end to confront David Bowie about your teddy
bear(?). Uh, nevermind the details, it will be cool, there will be
muppets. (as pictured)

The first part of the problem is to do the most basic case. JC is at
the top of the maze and DB is at the bottom. We want to find all paths
to get her there. I like this problem a bunch because apparently it's
on a lot of problem sets, so pretty much everyone gets it. There's a
slick math way to do it, but let's stick to code.

> type Pos = (Int, Int)

> maze1 :: Pos -> Int
> maze1 (0, _) = 0
> maze1 (_, 0) = 0
> maze1 (1, 1) = 1
> maze1 (r, c) = maze (r-1, c) + maze (r, c-1)

I always like to start totally naive. How many paths are there? We'll
just take the down paths and the right paths and add them up.

So obviously this solution has some problems, like the fact that it is
exponential in practice. We end up calling maze1 (1,1) ~ (n+m choose
m) times which is really bad news.

From here most people realize that it is dynamic programming,
restructure the problem, and start from the beginning with lots of
state. We can cheat though and keep our code.

> import qualified Data.MemoCombinators as Memo
>
> maze2 :: Pos -> Int
> maze2 = Memo.integral maze2'
>     where maze2' (0, _) = 0
>           maze2' (_, 0) = 0
>           maze2' (1, 1) = 1
>           maze2' (r, c) = maze2 (r-1, c) + maze2 (r, c-1)

maze2 uses a cache table (memoization) to save the results and save us from
exponential blowup.

But David Bowie is not going to stop at this kind of basic
maze, he is after all the king of all goblins. He'll probably have all
sorts of crazy things in our way before the final battle. Like spiked walls or quicksand or
the final battle.

> data Obstacle = SpikedWall | QuickSand Int | FinalBattle
> atPos :: Pos -> Obstacle

atPosition defines the obstacles in the maze.

> maze3 :: Pos -> Int
> maze3 = Memo.integral maze3'
>     where
>       maze3' pos = case atPos pos of
>       SpikedWall  => 0
>       FinalBattle => 1
>       QuickSand _ => maze3 (r-1, c) + maze3 (r, c-1)

This is pretty cool. We added new features but the code got less
complex. Spiked wall generalized both of the corner cases that we had
to deal with previously.

----------------

Now at this point in the question. I like to switch it up. We've so
far ignored the quicksand and the fact that it makes some paths more
expensive than others. Let's call the cost of quicksand its
stickiness. What we'd really like to do is find the
least sticky path to the FinalBattle.

Since this is a shortest-path question, it is tempting here to abandon
dynamic programming and switch to straight depth-first search. But dfs
gives us the same retracing issues that we had on the first
problem. So let's stick what we have and see what it gives us. The
final battle is the goal, so that's free. Spiked walls are really
sticky, being as they are spiked, so that's infinitely bad. And
quicksand has different sticky depending on which square it is.

> maze4 :: Pos -> Int
> maze4 = Memo.integral maze4'
>     where
>       maze4' pos = case atPos pos of
>       SpikedWall  => infinity
>       FinalBattle => 0
>       QuickSand stickiness => stickiness + (maze4 (r-1, c) `min` maze4 (r, c-1))

Ok, so this is really cool. Two different problems over the same grid
with very similar solutions. In fact, they are almost identical, with
a few numbers flopped.

We can fix this. We just need to find the similarities. We need a
value for the SpikedWall fail (let's call it zero), a FinalBattle
success (let's call it one), a way to combine previous values (let's
call it <*>), and a way to add in new cost (let's call it <+>). This
whole set of operations forms what is called a SemiRing. I like the
name SemiRing, it seems fitting that our Labyrinth quest should come
down to some magical ring.

> class SemiRing a where
>     zeroS :: b -> a
>     oneS  :: b -> a
>     <+>   :: a -> a -> a
>     <*>   :: a -> a -> a
>     toSemiRing :: Int -> b -> a

We can use this to generalize our previous solutions.

> maze5 :: (SemiRing a) => Pos -> a
> maze5 = Memo.integral maze5'
>     where
>       maze5' pos = case atPos pos of
>       SpikedWall  => zeroS pos
>       FinalBattle => oneS pos
>       QuickSand stickiness => (toSemiRing stickiness pos) <*> (maze5 (r-1, c) <+> maze5 (r, c-1))


Now we need to define instances that match our two previous
problems. The first was to find all paths -

> newtype AllPaths = AllPaths Int
> instance SemiRing a => AllPaths where
>     zeroS _ = AllPaths 0
>     oneS  _ = AllPaths 1
>     (AllPaths c1) <+> (AllPaths c2) = AllPaths (c1 + c2)
>     _ <*> current  = current
>     toSemiRing v _ = AllPaths v

Notice how we need the <*> operation for this first guy to be not cummatative (a
<*> b != b <*> a), hence why this is semi.

The second guy is really similar-

> newtype ShortPath = ShortPath Int
> instance SemiRing a => ShortPath where
>     zeroS _ = ShortPath infinity
>     oneS _  = ShortPath 0
>     (ShortPath c1) <+> (ShortPath c2) = ShortPath (c1 `min` c2)
>     (ShortPath c1) <*> (ShortPath c2) = ShortPath (c1 + c2)
>     toSemiRing v _ = ShortPath v

So this is pretty cool, but it doesn't really buy us much. With the
instance declarations, we have pretty much the same code that we had
before. It really comes in handy if we want to do crazier things. For
instance, the next question that I ask is to give me back the shortest
path from the the maze. The first instinct is to augment the clean
function with all kinds of extra arg passing, but we can get this done
in the SemiRing itself.

> newtype Path = [Pos]
> newtype AugShortPath = AugShortPath {cost::Int, path::Path}
> instance SemiRing a => AugShortPath where
>     zeroS pos = AugShortPath infinity [pos]
>     oneS pos  = AugShortPath 0 [pos]
>     s1 <+> s2 = if cost s1 < cost s2 then s1 else s2
>     s1 <*> s2 = AugShortPath (cost s1 + cost s2) (path s1 ++ path s2)
>     toSemiRing v pos = AugShortPath v [pos]

This SemiRing does everything that the ShortPath semiring does, but is
augmented with the path that has the smallest value. It's pretty cool
that we were able to solve a more interesting problem without every
even touching maze5.

-----------------

Let's keep on going. Now let's say we want to be able to move
diagonally in our maze. We could change our function to take a third
step, say maze5 (i-1, j-1) each time, but that is getting a bit
messy. Instead, let's work on the graph structure.

> class Maze a where
>     type Node :: *
>     children  :: a -> Node -> [Node]
>     atPos     :: a -> Node -> Obstacle


> newtype SquareMaze = SquareMaze (Pos->Obstacle)
> instance Maze SquareMaze where
>     type Node = Pos
>     children _ (Position (i,j)) =
>         map Position [(i-1, j), (i, j-1), (i-1, j-1)]
>     atPos (SquareMaze maze) (Position (i,j))  = maze (i,j)


> maze6 :: (Maze a, SemiRing b) => a -> b
> maze6 maze = mmaze
>     where
>       mmaze = Memo.integral maze6'
>       maze6' node = case atPos maze node of
>       SpikedWall  => zeroS node
>       FinalBattle => oneS node
>       QuickSand s => (toSemiRing s node) <*>
>                     (fold <+> nextNodes $ zeroS node)
>           where nextNodes = map mmaze $ children maze node

So that's pretty cool. a super general maze solver in about 6 lines of
codes, with maybe 10 lines of plumbing to back it up. Clearly there
are some issues, it would be nice to have it be breadth first
search. There are some cool ways to get that, but maybe I'll leave
that for another entry.

