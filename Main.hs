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
    printer 5 $ playerName ++ " 님 안녕하세요!"
    hSetBuffering stdin NoBuffering
    printer 5 "녹색물산 관리자가 되신 것을 환영합니다."
    printer 5  "그럼 시작하겠습니다. ... 엔터 키를 누르세요."
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

sePP_ :: IO ()
sePP_ = mapM_ putStrLn ["                             ███████████████████████                   "
                            ,"                 ██████████                           ████████         "
                            ,"     ███████                                                           "
                            ,"                                                                       "
                            ,"    █                                                                  "
                            ,"           ███                                         ███     █       "
                            ,"           ███                                         ███             "
                            ,"   █       ███                                         ███      █      "
                            ,"   █       ███                                         ███             "
                            ,"   █       ███                                         ███             "
                            ,"   █       ███                                                         "
                            ,"   █                            ██████                                 "
                            ,"   █                                                                   "
                            ,"   ██                                                            █     "
                            ,"   ██████                                                  ██████      "
                            ,"           ██████████  ████████████████████                            "
                            ,"                                                                       "
                            ,"                                                                       "
                            ,"                                                                       "
                            ,"                                                                       "
                            ,"                                                        ████  ████     "
                            ,"                                                       ████████████    "
                            ,"                                                       ████████████    "
                            ,"                                                       ███████████     "
                            ,"                                                         ████████      "
                            ,"                                                           ████        "
                            ,"                                                                       "]

seDie :: IO ()
seDie = mapM_ putStrLn[      "                          ████████████████████                         "
                            ,"                     ░████▓░░                 ▓███▒███████▓▓▓▓▒▒▓█░    "
                            ,"    ██████████████▓███▓▒                                         ▒     "
                            ,"    █                                                   ░        ▓     "
                            ,"    █▒                                                          ▓      "
                            ,"    █▓░                        ░░░░░░░░░░░░░░░                 ▒       "
                            ,"    ▒█░             ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░          ▒        "
                            ,"     █▒ ░    ░    ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░         ░         "
                            ,"     ░█░ ░░░░     ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░▒▓░░░░        ▓         "
                            ,"      ██░    ░░░  ░░░░░█▒░░░░░░░░░░░░░░░░░░░░░░▒▒░░░░        ░░        "
                            ,"       █▓░░ ░░░░░ ░░░░░▓▒░░░░░░░░░▒██░░░░░░░░░░▒▓░░░░        ░▓        "
                            ,"        █▒░░░░  ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░        ▓        "
                            ,"        ▒▓▓░ ░░░░ ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░        ▓        "
                            ,"        █▓▓▓▒░░░░ ░▒▒▒░▒░░░░░░░░░░░░░░░░░░░░░░░░░░▒▒▒░        ▒▒       "
                            ,"        █▒▒▒░░░░░░  ░░░░░░░░░░░░░░░░░░░░░░░░░░░               ░▓       "
                            ,"        █▒▒▒░▒▒░░░░░░░░░░░░   ░░ ░░░░░░░░░░░                   ░       "
                            ,"      ▓█▓▒▓▒░▒▒░░░░░░░░░░░░                        ░▒          ░████   "
                            ,"    ▒██▓▓░▒▓░▒▒░░░░░░░░░░░░░ ░     ░░░░        ▓████▓▒         ▒▒▒▓▓██ "
                            ,"   █▓▒░    ▒ ░▒                                 ░░░░░          ░░░   ░ "]

se :: IO ()
se = mapM_ putStrLn [
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
    printer 3 "2047년의 평범한 어느 날."
    printer 2 "당신은 언제나처럼 평화롭게 잠을 자고 있었다."
    entertoContinue
    printer 5 $ player ++ ": 부스럭 부스럭.."
    printer 5 $ player ++ ": 드르렁... 드르렁..."
    entertoContinue
    delayDisplayStrLn 100 "띵 "
    printer 1 "\n"
    entertoContinue
    se
    mapM_ (printer 5) ["비서 로봇: 관리자님!"
                    , "비서 로봇: 시끄럽습니다."
                    , player ++ ": 흠냐.."
                    , player ++ ": 깨우지 마.."
                    , "비서 로봇: 관리자님."
                    , "비서 로봇: 회사를 방치하고 하루 종일 잠만 자는 건 좋습니다만"
                    , "비서 로봇: 저 시끄러운 알림은 도대체 뭡니까?"
                    , "비서 로봇: 제발 좀 멈춰 주세요! 저는 권한이 없다고요."]
    delayDisplayStrLn 10 " "
    delayDisplayStrLn 200 "띵"
    entertoContinue
    delayDisplayStrLn 10 " "
    entertoContinue
    mapM_ (printer 5) [player ++ ": 뭐야.. 무슨 알림이지?"
                            , player ++ ": 오늘 같은 평화로운 주말에.."]
    entertoContinue
    printer 10 "비서 로봇: 관리자님, 오늘은 월요일입니다."
    printer 3 "0: 비서 로봇을 망치로 때린다. \n 1: 비서 로봇을 뿅망치로 때린다. \n 2: 비서 로봇을 쓰다듬는다."
    select [seDead player]



select ::[IO ()] -> IO ()
select ios = do
    num <- getLine
    let int = read num ::Int
    case ios !? int of
        Just io -> io
        Nothing -> putStr ("0 부터 " ++ show (length ios + 1) ++ " 중에 고르세요 : ") >> select ios


(!?) :: [a] -> Int -> Maybe a
xs !? n
    | n >= 0 && n < length xs = Just (xs !! n)
    | otherwise = Nothing


entertoContinue :: IO ()
entertoContinue = do
    putStrLn "(엔터를 눌러 진행...)"
    getLine
    putStr "\ESC[A\ESC[A\r                                 \n"
    putStr "\n"


inf :: String -> IO ()
inf str = do
    sequence_ $ repeat (delayDisplayStr str)

delayDisplayStrLn :: Int -> String -> IO ()
delayDisplayStrLn count str = do
    threadDelay count
    Control.Monad.replicateM_ count $ delayDisplayStr str
    putStr "\r"
    putStrLn $ str ++ "                  "


delayDisplayStr :: String-> IO ()
delayDisplayStr str = do
    threadDelay 10000
    putStr str

sceneNotOpened :: String -> IO()
sceneNotOpened player = do
    mapM_ (printer 7) ["비서 로봇: 관리자님, 쫄으셨습니까? ㅋㅋ"
                            , player ++ ": 아니, 전에 이런 거 와서 눌렀는데 그 이후로 하루 종일 이상한 문자가 온다니까?"]
    entertoContinue
    delayDisplayStrLn 100 "띵띵"
    mapM_ (printer 7) ["비서 로봇: 으악! 그냥 빨리 눌러요! "
                            , player ++ ": ... 알았어."]
    entertoContinue

seDead :: String -> IO ()
seDead player = do
    seDie
    printer 5 $ player ++ "의 망치에 맞아서 비서 로봇이 죽었습니다.."
    printer 5 "전부 당신 탓입니다."
    entertoContinue
    printer 5 "다시 하시겠습니까?"
    printer 5 "0: 다시 한다."
    select [startGame]

sePP :: String -> IO ()
sePP player = do
    sePP_
    printer 5 "관리자님, 그 장난감 좀 갖다 버리십시오."

printer :: Int -> String -> IO ()
printer sec (s:ss) = do
    putStr [s]
    threadDelay $ 10000 * sec
    printer sec ss
printer _ _ = putStrLn ""
