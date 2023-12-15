module InputTest where
import Test.HUnit (test, Test(TestLabel, TestCase), assertEqual)
import Common (Solution(solutionRun, solutionName), NoSolution(..))
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25


inputTest solution expected = let name = solutionName solution in
    TestLabel name $ TestCase (do input <- readFile $ "data/" ++ name ++ "-input.txt"
                                  assertEqual "" expected (solutionRun solution input))

tests = TestLabel "InputTest" $ test
    [ inputTest Day01.solution (232, 1783)
    , inputTest Day02.solution (1606483, 3842356)
    , inputTest Day03.solution (2565, 2639)
    , inputTest Day04.solution (254575, 1038736)
    , inputTest Day05.solution (258, 53)
    , inputTest Day06.solution (400410, 15343601)
    , inputTest Day07.solution (46065, 14134)
    , inputTest Day08.solution (1333, 2046)
    , inputTest Day09.solution (251, 898)
    , inputTest Day10.solution (360154, 5103798)
    , inputTest Day11.solution ("hxbxxyzz", "hxcaabcc")
    , inputTest Day12.solution (111754, 65402)
    , inputTest Day13.solution (664, 640)
    , inputTest Day14.solution (2655, 1059)
    , inputTest Day15.solution (21367368, 1766400)
    , inputTest Day16.solution (40, 241)
    , inputTest Day17.solution (4372, 4)
    , inputTest Day18.solution (814, 924)
    , inputTest Day19.solution (518, 200)
    , inputTest Day20.solution (831600, 884520)
    , inputTest Day21.solution (78, 148)
    , inputTest Day22.solution (NoSolution, NoSolution)
    , inputTest Day23.solution (NoSolution, NoSolution)
    , inputTest Day24.solution (NoSolution, NoSolution)
    , inputTest Day25.solution (NoSolution, NoSolution)
    ]
