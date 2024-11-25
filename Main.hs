module Main (main) where
import Control.Concurrent
import System.Process
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
    delayPutStrLn 5  "안타깝지만 ANCI 는 없습니다. 모든 줄바꿈은 되돌릴 수 없습니다.(제가) 그러므로 모든 줄바꿈은 기록됩니다."
    delayPutStrLn 5  "그럼 시작하겠습니다. ... 엔터 키를 누르세요."
    hFlush stdout
    getLine

    sequence_ prototype

delayPutStrLn :: Int -> String -> IO ()
delayPutStrLn count str = do
    threadDelay count
    Control.Monad.replicateM_ count delayDisplay
    putStr "\r"
    putStrLn $ str ++ "                     "

delayDisplay :: IO ()
delayDisplay = do
    threadDelay 100000
    putStr "* "

prototype :: [IO ()]
prototype = map putStrLn [ " █████████████ ██ ████████████ █ ███ ██ ███████████   ████████████████ "
                            ," █████████████ █ █ ███████████ ██ ███ ██ ██████████ █ ████████████████ "
                            ," ████████████  █ █  ██████████ █ █████ █  █████████ ██  ██████████████ "
                            ," ██  ██  ████  █  █ ██████████ █ █████  █  ████████  ███ █████████████ "
                            ," █ ██  █  █       █     █ █████ █ █ ███  █ █████████ █ ██ ████████████ "
                            ," █          ██████ ████     ███ █ ██ ███ ██  ███████ █ ██ ████████████ "
                            ,"                              █ ██ ██ ██████████████ █ ███  ██████████ "
                            ,"          ██  ██          █      █  █ ███  █  ██████ █  ██  ██████████ "
                            ,"           █   █                  █  █      █  █████ ██  █  █ ████ ███ "
                            ,"           ██            ██       ██  ██  █  █        █  █  ██   ██  █ "
                            ,"      █    █  ██       █ ███   ██  ██     █████       █  █ █  ████ █ █ "
                            ,"      ██   ██ ███████████ █ ██      █       ███████████  █ █     █ █ █ "
                            ,"       █  █ █ ██████████                    ██████████   █ █     █ █   "
                            ,"       █  █ █ ██████████   █                  ███████    █ ███  █  ██  "
                            ,"        █ ███    ████                                 ██ █ ███  █ ███  "
                            ,"        ██ ██           █                   █████  ███   █ ██    ████  "
                            ,"         █ ███ ██████                                    █ █    █████  "
                            ," █       ██ ██                                           █ ███████ ███ "
                            ," █        █ █                                            █ █████ █  ██ "
                            ," ██       ██ █                                           █ █████ █ ███ "
                            ,"  ███     ██                                            ██ ███████████ "
                            ," █  ███    █                                           ███ ████ ███ ██ "
                            ,"     ██ █  █                                          ██ █ █ ██ ███ ██ "
                            ," ██  ██   █ █  █                   █                ████ █ ███  ██████ "
                            ," ███ ██    ██   █                                 ██████ ███ █ ███████ "
                            ," ███  █     █ ██ ██                           ██ ███████ ██ ██ ███████ "
                            ," ████ █     ██  █ █ ██                      █      █     ██ ██ ███████ "
                            ,"  ███ █      █  ███ ██████ █            ██      █       ██ ██  █████ █ "
                            ,"  ██  █       █ █  █████████      ██            █ █     █   █ ██████ █ "
                            ,"  ██  █      █  ███ ██████                      ███          █████████ "
                            ,"  ██ ██        ██ ████ ███                       █          ████████   "
                            ,"     ██        █                               █ █                  ██ "
                            ,"     ██                                                                " ]


