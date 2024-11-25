module Main (main) where
import Control.Concurrent
import System.Process
import Data.List (elemIndex)
import Control.Monad
import System.IO
main :: IO ()
main = startGame

startGame :: IO ()
startGame = do
    putStr "인코딩 설정 중.. 글자가 깨지면 사용자 문제입니다."
    system "chcp 65001"
    hSetEncoding stdout utf8
    putStr "이름을 입력하세요 : "
    playerName <- getLine
    delayPutStrLn 5 $ playerName ++ " 님 안녕하세요!"
    hSetBuffering stdin NoBuffering
    delayPutStrLn 5 "녹색물산 관리자가 되신 것을 환영합니다."
    delayPutStrLn 5  "그럼 시작하겠습니다. ... 엔터 키를 누르세요."
    hFlush stdout
    entertoContinue
    scene0 playerName

delayPutStrLn :: Int -> String -> IO ()
delayPutStrLn count str = do
    threadDelay count
    Control.Monad.replicateM_ count delayDisplay
    putStr "\r"
    putStrLn $ str ++ "                  "

delayDisplay :: IO ()
delayDisplay = do
    threadDelay 100000
    putStr "* "

prototype0 :: IO ()
prototype0 = putStrLn ""

security :: IO ()
security = mapM_ putStrLn [
                                        "                            ▒██████████████▒                           "
                                        ,"                       █████▓▒░            ▒▓████    ▓████████████     "
                                        ,"     ▓▓██▓▓▒░       ████▒░                      ░▒▓▒▒░░░░        ▓     "
                                        ,"    █▒░░░░░░▒▒▓▓████▓▒░                                          ▓     "
                                        ,"    █▒                                                          ▓      "
                                        ,"    ██░                                                        ▒       "
                                        ,"     █░░                ░░░░░░░░░░░░░░░░░░░░░░░░░░░           ▒        "
                                        ,"     █▓ ░          ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░         ░░        "
                                        ,"      █░ ░  ░░    ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░         ▓         "
                                        ,"      ██░ ░░░     ░░░░░█░░░░░░░░░░░░░░░░░░░░░░░██░░░░        ▓         "
                                        ,"       █▓░  ░ ░▒░ ░░░░▒▒▓░░░░░░░░░░░░░░░░░░░░░░░░░░░░        ▒░        "
                                        ,"        █▒ ░░░    ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░        ░▓        "
                                        ,"         █▒░ ░░░░ ░░░░░░░▒█████▓█████▓██▒██▓██░░░░░░░░        █        "
                                        ,"        ░█▓▓░░░░░ ░▒░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░▒░        ▓        "
                                        ,"        ▓▓▒▓▒░░░░ ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░          ▓        "
                                        ,"        ▓▓▓▒░▒▓░░░░░░░                                        ▒        "
                                        ,"        ▒▒▓▒░▒▓░░░░░░ ░░░░░                                    ▒█▒     "
                                        ,"      ███▒▒▓░▒▓░░░░░░░░░░░ ░        ░          ░▓░░█▓░         ▒▓████  "
                                        ,"    ▓█▓░░  ▒░░▒                                ░░░░░░          ▒░░░░▒▓ "]

prototype :: IO ()
prototype = mapM_ putStrLn [ "                   ▓                       ▓                                     "
                            ,"░                 ▓▓               ▓                                            "
                            ," ░ ░              ▓▓               ▓▓                                           "
                            ,"░░░░              ▓▒                ░       ▓                                   "
                            ,"░░░░░░  ░  ░    ░  ▒▓               ░▓       ▓                                  "
                            ,"░░░░░░░░░░░░░░░░░░▒▒▒░░░░░░░        ▓▒▓       ▓             ▓ ▓                 "
                            ,"░░░░░░░░░░░░░░░░░░▒▒▒ ░░░░░░░░       ▒▒                     ▓ ▓▓                "
                            ,"▒░░░░░░▒░░░░░░ ░░ ▒░░░ ▒▒░░░░░░░     ▓▒▒                    ▓  ▓▓               "
                            ,"▒▒▒▒▒░░░░░▒░░░░░░ ▒░░░░▒▒▒▒░░░░░░░    ▒▒░                                       "
                            ,"▒▒▒▒▒▒▒░░░▒▒▒░░░░ ▒░░  ▒▒▒▒▒░░░░░░░░░░ ▒▒      ▒▒▒              ▓▓              "
                            ,"▒▒▒▒▒▒▒▒░░▒░▒ ░░▒░░░    ▒▒▒▒▒░░░░░░░░░░ ▒▒ ░ ▒▒▒▒▒░                 ▓           "
                            ,"▒▒▒▒▒▒▒ ░░▒▒▒ ░▒▒▒░░░  ░░▒▒▒▒ ▒▒░░░░░░░░▒▒░▒▒▒░   ▓▓▓        ▓      ▓▒▒▒▒▒▒▓    "
                            ,"▒▒▒▒▒▒▒░▒░▒▒░ ░░   ▒▒▒▒░ ░ ▒▒▒▒▒▒▒▒▒░░░░░▒░░░░░▒▒▒▓▓▓▓    ▓▓▓▓        ░░░░ ▒    "
                            ,"▒▒▒▒▒▒▒▒▓▒░▒░░░░░▓▓▓▓▓▓▓▓▓▓  ▒░░ ▒▒▒▒▒▒▒▒▒░░░░░░░▒ ▓▓▓▓▓▓▓▓▓▓▓       ░░▒▒░░▒▓   "
                            ,"▒▒▒▒▒▒▒▒▒▒▒▒░▒ ░ ▓▓▓▓▓▓▓▓   ░▒▒░   ░░░░░░░          ▓▓▓▓▓▓▓▓▓▒▒▒     ░░▒▒░▒▒▓   "
                            ,"▒▒▒▒▒▒▒▒▒▓▒▒▓░▓▒           ▒   ▒                   ▒  ▓▓▓▓ ▒░▒▒▒▒   ░ ░▒▒ ▒ ▓   "
                            ,"▒▒▒▒▒▒▒▒▒▒▒▒▒▒ ▒░░▒    ░▒░                            ░░ ░░░░▒ ▒▒   ░  ▒░▒▒     "
                            ," ▒▒▒▒▒▒▒▒▒ ▒▒ ▒▒▒░         ▒░                     ░░░▒▒▒▒▒▒▒▒▒▒▒▒    ▒▒▒▒▒▓     "
                            ,"▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░▒▒▒░░░░░░                      ░░░░░░░░▒▒▒▒▒▒   ▒▒▒▒     ▓  "
                            ,"▒▒▒▒▒▒▒▒▒▒▒ ▒▒  ░░░░░░░░░░░░           ▒                   ░░░░▒▒    ▓▓   ▓  ▓  "
                            ,"▒▒ ▒▒▒▒▒▒▒▒▒▒▒▒  ░░░░░░░░                                   ░░▒▒▒         ▓     "
                            ,"░▒▒▒▒▒▒▒▒▒▒▒ ▒▒                                             ░░▒▒          ▓     "
                            ,"▒▒▒▒▒ ▒▒▒▒▒▒▒▒▒▒▒                                          ░░▒▒    ▓            "
                            ,"▒▒▒▒▒▒▒ ▒▒▒▒▒ ▒▒▒░                                        ░░░▒▓    ▓            "
                            ,"▒▒▒▒▒▒▒ ▒ ▒▒▒▒▒▒▒░                                       ░░▒▒▓     ▓     ▓    ▓ "
                            ,"▒▒▒▒▒▒▒ ▒▒▒▒▒▒░▒▒▒░                 ░▒▒▒▒▒▒░           ░░░▒ ▓            ▓    ▓ "
                            ,"▒▒▒▒▒▒▒ ▒▒▒▒▒░ ▒▒▒▒▒▒▒                               ░░░▒         ▓           ▓ "
                            ,"▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ ▒▒ ▒▒▒▒▒                           ░▒ ▒   ▓            ▓     ▓ "
                            ,"▒▒▒▒▒▒▒▒▒▒▒▒▒▒  ▒▒ ▒ ░▒▒▒▒▒▒                      ░░░░░                       ▓ "
                            ,"▒▒▒▒░▒▒▒▒▒▒▒▒▒▒▓▒▒▒░░ ░ ▒▒▒▒▒▒▒▒               ░░░░░░░░ ░░░░░░   ▓     ▓      ▓ "
                            ,"▒▒▒░▒▒▒ ▒▒▒▒▒▒▒▒▒▒ ░░   ░░▒▒▒▒▒▒░░░░░▒▒▒░ ▒░ ░░░░░░░░░░░░ ░░░░░░▓               "
                            ,"▒▒▒▒▒▒▒▓░▒▒▒▒▒▒▒▒▒ ░░▒░ ░░░▒▒▒▒▒    ░░░░░░    ░░░░░░░░░  ░░░░░░░░░░░░           "
                            ,"▒▒▒▒▒▒▒▓ ▒▒▒▒▒▒▒▒▒ ▒▒▒▒ ▒▒▒▒▒▒░▒               ░░░░░░░ ░ ░░░░░░░░░░▒▒▒        ░ "
                            ,"▒▒▒▒░▒▓▓ ▒▒▒▒▒▒▒▒▒▒▒▒▒▓▒▒▒▒▒░  ░                  ░░░░ ░ ░░░░░░░▒▒▒▒▒░▒▓▓▒▒▒▒▓▒▓"
                            ,"▒▒▒▒▒▒▓▓ ▒▒▒▒▒▒▒▒▒▓▒░  ▒▒      ░                   ░░░░░░░░░░░▒▒▒▒▒▒       ▒▒▓▓▒" ]


scene0 :: String -> IO ()
scene0 player = do
    delayPutStrLn 3 "2047년의 평범한 어느 날."
    delayPutStrLn 2 "당신은 언제나처럼 평화롭게 잠을 자고 있었다."
    entertoContinue
    delayPutStrLn 5 $ player ++ ": 부스럭 부스럭.."
    delayPutStrLn 5 $ player ++ ": 드르렁... 드르렁..."
    entertoContinue
    delayPutStrLnStr 100 "띵 "
    delayPutStrLn 1 "\n"
    entertoContinue
    security
    mapM_ (delayPutStrLn 5) ["비서 로봇: 관리자님!"
                    , "비서 로봇: 시끄럽습니다."
                    , player ++ ": 흠냐.."
                    , player ++ ": 깨우지 마.."
                    , "비서 로봇: 관리자님."
                    , "비서 로봇: 회사를 방치하고 하루 종일 잠만 자는 건 좋습니다만"
                    , "비서 로봇: 저 시끄러운 알림은 도대체 뭡니까?"
                    , "비서 로봇: 제발 좀 멈춰 주세요! 저는 권한이 없다고요."]
    delayPutStrLnStr 10 " "
    delayPutStrLnStr 200 "띵"
    entertoContinue
    delayPutStrLnStr 10 " "
    entertoContinue
    mapM_ (delayPutStrLn 5) [player ++ ": 뭐야.. 무슨 알림이지?"
                            , player ++ ": 오늘 같은 평화로운 주말에.."]
    entertoContinue
    delayPutStrLn 10 "비서 로봇: 관리자님, 오늘은 월요일입니다."
    select [] --todo

select ::[IO ()] -> IO ()
select ios = do
    num <- getLine
    let int = read num ::Int
    case ios !? int of
        Just io -> io
        Nothing -> putStr "0~3 중에 고르세요 : " >> select ios


(!?) :: [a] -> Int -> Maybe a
xs !? n
    | n >= 0 && n < length xs = Just (xs !! n) 
    | otherwise = Nothing  


sceneSelect :: Int -> [IO ()] -> IO ()
sceneSelect int ios = ios !! int

entertoContinue :: IO ()
entertoContinue = do
    putStrLn "(엔터를 눌러 진행...)"
    getLine
    putStr "\ESC[A\ESC[A\r                                 \n"
    putStr "\n"


inf :: String -> IO ()
inf str = do
    sequence_ $ repeat (delayDisplayStr str)

delayPutStrLnStr :: Int -> String -> IO ()
delayPutStrLnStr count str = do
    threadDelay count
    Control.Monad.replicateM_ count $ delayDisplayStr str
    putStr "\r"
    putStrLn $ str ++ "                  "


delayDisplayStr :: String-> IO ()
delayDisplayStr str = do
    threadDelay 10000
    putStr str

sceneNotOpened :: IO()
sceneNotOpened = do
    delayPutStrLn 5 ""