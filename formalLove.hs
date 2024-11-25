module Main (main) where
import Control.Concurrent
import System.Process
import Data.List (elemIndex)
import Control.Monad
import System.IO
import Distribution.Compat.Prelude (readMaybe)
main :: IO ()
main = startGame



select ::[IO ()] -> IO ()
select ios = do
    num <- getLine
    let int = readMaybe num :: Maybe Int
    case (ios !?) =<< int of
        Just io -> io
        Nothing -> putStr ("0 부터 " ++ show (length ios - 1) ++ " 중에 고르세요 : ") >> select ios

superSelect ::[Int] -> String-> [IO ()] -> IO ()
superSelect is player ios = do
    num <- getLine
    let int = readMaybe num :: Maybe Int
    let jint = Just int
    case (ios !?) =<< int of        
        Just io ->
                if length is == 3 then sceneFinal player else
                    (if int `elem` jint then putStr "안 가 본 곳으로 가십시오." >> select ios else io)
        Nothing -> putStr ("0 부터 " ++ show (length ios - 1) ++ " 중에 고르세요 : ") >> select ios



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
    putStrLn ""


delayDisplayStr :: String-> IO ()
delayDisplayStr str = do
    threadDelay 10000
    putStr str


printer :: Int -> String -> IO ()
printer sec (s:ss) = do
    putStr [s]
    threadDelay $  5000 * sec
    printer sec ss
printer _ _ = putStrLn ""


startGame :: IO ()
startGame = do
    system "chcp 65001"
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering
    putStrLn "인코딩 설정됨.. 글자가 깨지면 사용자 문제입니다."
    putStrLn "v0.0.1"
    putStrLn "이 게임은 테스트되지 않았습니다. \n문제가 발생했다면 https://github.com/NOT2ho/formalLove-trial 이나 https://x.com/MELC0chopper 로 알려 주십시오."
    putStrLn ""
    entertoContinue
    putStr "이름을 입력하세요 : "
    playerName <- getLine

    printer 10 "..."
    printer 5 $ playerName ++ " 님 안녕하세요!"
    printer 10 "이 게임은 <사랑은 포르말린에 담가서> 의 텍스트 버전 체험판입니다."
    printer 2 "본게임은 현재 펀딩 중이며 https://tumblbug.com/formallove 에서 볼 수 있습니다! "
    entertoContinue
    printer 20 ". . ."
    printer 5 $ playerName ++ " 님..."

    printer 5 "녹색물산 관리자가 되신 것을 환영합니다!"
    printer 5  "그럼 시작하겠습니다. ...."
    printer 20 ". . ."
    printer 5  "0을 누르면 처음부터, 1을 누르면 중간 선택지부터 시작입니다."
    putStr "선택: "
    select [scene0 playerName, superSelect [] playerName [scenePrototype [] playerName, sceneJeong [] playerName, sceneBroad [] playerName]]

    
  

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
    printer 3 "0: 비서 로봇을 망치로 때린다. \n1: 비서 로봇을 뿅망치로 때린다. \n2: 비서 로봇을 쓰다듬는다."
    putStr "\n선택: "

    select [seDead player, sePP player, seLove player]


seDead :: String -> IO ()
seDead player = do
    seDie
    printer 5 $ player ++ "의 망치에 맞아서 비서 로봇이 죽었습니다.."
    printer 5 "전부 당신 탓입니다."
    entertoContinue
    printer 5 "다시 하시겠습니까?"
    printer 5 "0: 다시 한다."
    putStr "\n선택: "

    select [startGame]

sePP :: String -> IO ()
sePP player = do
    sePP_
    printer 5 "비서 로봇: 관리자님, 그 장난감 좀 갖다 버리십시오."
    scene_ player


seLove :: String -> IO ()
seLove player = do
    seLoved
    printer 20 "비서 로봇: 헤에.. 관리자님."
    scene_ player

scene_ :: String -> IO ()
scene_ player= do
    delayDisplayStrLn 300 "띵"
    printer 5 $ player ++ ": 대체 무슨 알림이지?"
    entertoContinue
    printer 10 "메시지: [(2048 신작 얼리액세스!! 당신에게 특권을 드립니다.) ]"
    entertoContinue
    printer 4 $ player ++ ": 이거 스팸 아냐?"
    putStrLn "0. 메시지를 연다. \n1. 메시지를 열지 않는다."
    putStr "\n선택: "

    select [scene1 player, sceneNotOpened player]


sceneNotOpened :: String -> IO()
sceneNotOpened player = do
    mapM_ (printer 7) ["비서 로봇: 관리자님, 쫄으셨습니까? ㅋㅋ"
                            , player ++ ": 아니, 전에 이런 거 와서 눌렀는데 그 이후로 하루 종일 이상한 문자가 온다니까?"]
    entertoContinue
    delayDisplayStrLn 100 "띵띵"
    mapM_ (printer 7) ["비서 로봇: 으악! 그냥 빨리 눌러요! "
                            , player ++ ": ... 알았어."]
    entertoContinue
    scene1 player

scene1 :: String -> IO ()
scene1 player = do
    mapM_ (printer 7) [ "메시지: 대전 격투 게임의 여왕! [퀸 오브 파이터즈] 시리즈가 2048년을 맞아"
                        , "메시지: [퀸 오브 파이터즈 48]을 출시하게 되었습니다."
                        , "메시지: 이 문자는 퀸 오브 파이터즈 46 최상위 랭킹 유저 중 플레이 타임 상위 0.1%에게만 발송되는"
                        , "메시지: 특별 프로모션 [얼리액세스 이벤트] 알림입니다."]
    entertoContinue
    mapM_ (printer 8) [ player ++ ": 뭐? 광고잖아?"
                        , player ++ ": 하지만.. 퀸오브파이터즈 48이라고?"
                        , player ++ ": 마침 46이 질리던 참인데.."
                        , player ++ ": 한번 해 볼까? "]

    printer 2 "메시지: 게임 칩은 이미 녹색물산으로 발송되었으니 안심하십시오."
    entertoContinue

    print $  player ++ ": 뭐?"
    printer 10 "띵동 띵동"

    entertoContinue
    se
    mapM_ (printer 7) [ "비서 로봇: 관리자님이 얼마나 많이 플레이하셨으면 주소까지 기억한답니까?"
                        , "당신은 게임 칩을 끼우느라 정신이 없다."]
    entertoContinue
    mapM_ (printer 7) ["비서 로봇: 에휴.."
                        , player ++ ": 뭐야, 인터넷 연결을 지원하지 않는다고?"
                        , "비서 로봇: 그 칩, 관리자님만 받으신 것 같은데요?"
                        , player ++ ": 어떡하지.."
                        , player ++ ": 난 누구랑 게임을 하지?"]
    entertoContinue
    printer 10 "비서 로봇: 저랑은 안 됩니다."
    threadDelay 100000
    delayDisplayStr "0: 비서 로봇을 망치로 때린다. \n1: 비서 로봇을 뿅망치로 때린다. \n2: 비서 로봇에게 게임을 하자고 한다."
    putStr "\n선택: "

    select [seDead player, scenePP player, sceneWith player]


scenePP :: String -> IO ()
scenePP player = do
    mapM_ (printer 7) [ player ++ ": 너랑은 안 해."
                        ,  "비서 로봇: 너무하시네요."
                        , "비서 로봇: 같이 하자고 하면 기꺼이 한 판 붙어 주려고 했는데. "]
    entertoContinue
    mapM_ (printer 7) [ player ++ ": 그 뭉툭한 손으로?"
                        , " 비서 로봇: 에이, 제 cpu는 기기에 직접 연결할 수 있어요."
                        , player ++ ": 뭐? 그거 핵 아냐?"
                        , "비서 로봇: 글쎄요."
                        , "비서 로봇: 굳이 인간 상대를 찾으신다면.. 프로토타입이나.. 윤리위원회라든지. 밖에 사람은 많지 않습니까?"]
    entertoContinue
    sceneOut player
sceneWith :: String -> IO ()
sceneWith player = do
    mapM_ (printer 5) ["비서 로봇: 으.. 안 해요."
                       ,  "비서 로봇: 나가서 다른 사람 찾아봐요, 프로토타입이나.. 윤리위원회라든지."]
    entertoContinue
    sceneOut player

sceneOut :: String -> IO ()
sceneOut player = do
        printer 20 "밖으로 나가는 중…"
        printer 10 "어디로 가시겠습니까?"
        delayDisplayStr "0. 프로토타입 방\n1. 윤리위원회 회의실\n2. 방송실"
        putStr "\n선택: "
        superSelect [] player [scenePrototype [] player, sceneJeong [] player, sceneBroad [] player]


scenePrototype ::  [Int] -> String -> IO ()
scenePrototype ints player = do
    printer 20 "프로토타입 방 앞에 도착한 당신."
    entertoContinue
    printer 20 "프로토타입을 부르세요."
    putStr "입력 기다리는 중.. : "
    str <- getLine
    prototype
    entertoContinue

    mapM_ (printer 10) ["프로토타입: 관리자님."
                        , "프로토타입: " ++ str ++ " 라고 하시는 거"]
    printer 30 "프로토타입: 제가 다 녹음했습니다."
    entertoContinue

    printer 10 $ "녹음기: " ++ str

    mapM_ (printer 5) [player ++ ": 으악!"
                        , player ++ ": 당장 꺼."
                        , player ++ ": 그건 왜 녹음한 거야!"]
    entertoContinue
    mapM_ (printer 5) ["프로토타입: 관리자님이 잠을 깨웠다는 증거입니다. 법정에 제출해야 해요."]
    entertoContinue

    printer 3 $ player ++ ": 뭐?"
    entertoContinue
    mapM_ (printer 5) ["프로토타입: 도대체 여긴 왜 오십니까? 지금 새벽 2시라고요." ]
    entertoContinue
    mapM_ (printer 5) [player ++ ": 2시?"
                          ,"프로토타입: 관리자님은 지금까지 자셨겠지만, 저는 지금까지 일하다가 이제 잤어요."]
    entertoContinue
    printer 5 $  player ++ ": 하지만 어제는 월요일이었는데?"
    entertoContinue
    prototypeDark
    entertoContinue
    printer 20 "프로토타입: 관리자님, 저는 주말에도 일해요."
    entertoContinue
    printer 5 "프로토타입: 그래서 왜 오신 겁니까?"
    printer 5 $ player ++ ": 그.. "
    mapM_ delayDisplayStr ["0: 같이 게임하자. "
            ,"1: 녹음 좀 지워 줘."
            ,"2: 너랑 놀고 싶어서."
            , "3: 프로토타입을 뿅망치로 때린다."]
    putStr "\n선택: "

    select [scenePrototype0 ints player, scenePrototype4 ints str player, scenePrototype1 ints player, scenePrototype3 player]

scenePrototype4 :: [Int] -> String-> String -> IO ()
scenePrototype4 ints str player = do
    mapM_  (printer 10) ["프로토타입: 음, 알겠습니다."
                        , "프로토타입: 대신에 같이 게임하는 건 안 됩니다." ]
    entertoContinue
    printer 5 $ player ++ ": 내가 무슨 말을 할지 어떻게 안 거지?"
    entertoContinue
    printer 5 "프로토타입: 관리자님이 이러는 게 한두 번입니까?"
    printer 5 "프로토타입이 녹음기를 튼다."
    delayDisplayStrLn 50 str
    entertoContinue
    printer 5 $ player ++ ": 알았어! 딴 사람 찾아볼게. 지워 줘."

    printer 10 "당신은 시무룩 불쌍해진 채로 쫒겨났다."
    printer 10 "어디로 가시겠습니까?"
    delayDisplayStr "0. 프로토타입 방\n1. 윤리위원회 회의실\n2. 방송실"
    putStr "\n선택: "
    superSelect (0 : ints) player [scenePrototype (0:ints) player, sceneJeong (0:ints) player, sceneBroad0 (0:ints) player]


scenePrototype0 :: [Int] -> String -> IO ()
scenePrototype0 ints player = do
    prototypeSmall
    entertoContinue
    mapM_ (printer 6) ["프로토타입: 이번엔 또 무슨 게임입니까?"
                    , "프로토타입: 이상한 격투 게임만 아니면 전 좋습니다."]
    entertoContinue
    mapM_ (printer 5) [player ++ ": 이상하..지 않으니까!"
                        , player ++ ": 이번에 새로 나왔대. 신작이.."
                        , "프로토타입: 또 퀸 오브 파이터즈입니까?"
                        , "프로토타입: 그거 저번에 방송실에서 같이 했는데, 관리자님이 저를 일방적으로 두들겨 패는 것이 온 회사에 생중계되지 않았나요?"
                        ]
    entertoContinue
    mapM_ (printer 5)  [player ++ ": 하지만 재밌었잖아!"
                        , "프로토타입: 관리자님만 재미있었겠죠."
                        , "프로토타입: 나가세요. 저 자야 해요."
                        ]
    entertoContinue
    printer 10 "당신은 시무룩해진 채로 쫒겨났다."
    printer 10 "어디로 가시겠습니까?"
    delayDisplayStr "0. 프로토타입 방\n1. 윤리위원회 회의실\n2. 방송실"
    putStr "\n선택: "
    superSelect (0 : ints) player [scenePrototype (0:ints) player, sceneJeong (0:ints) player, sceneBroad0 (0:ints) player]

scenePrototype1 :: [Int] ->  String -> IO ()
scenePrototype1 ints player = do
    prototypeSmile
    mapM_ (printer 6) ["프로토타입: 네?"
                    , "프로토타입: 좋아요.. 하지만"]
    printer 20 "프로토타입: 퀸 오브 파이터즈는 안 됩니다"
    entertoContinue
    mapM_ (printer 5)  [player ++ ": 뭐?"
                        , "프로토타입: 관리자님이 양학하시잖아요."
                        , "프로토타입: 그런 뉴비배척겜 안 합니다."
                        ]
    entertoContinue
    mapM_ (printer 5)  [player ++ ": 시무룩.."
                        , "프로토타입: 다른 거 하고 놀래요?"]

    mapM_ putStrLn [[]
            , "0: 그래. "
            ,"1: 아니."]
    putStr "\n선택: "
    select [scenePrototype2_0 player, scenePrototype2_1 ints player]


scenePrototype2_0 :: String ->  IO ()
scenePrototype2_0 player = do
    printer 10 "프로토타입: 세계의 징그러운 식물 도감이 어디에 있지.."
    entertoContinue
    printer 20 $ player ++ ": ...?"
    printer 10 "프로토타입: 찾았다! 관리자님, 같이 이거 읽어요."
    entertoContinue
    printer 30 "........"
    printer 5 "그렇게 관리자와 프로토타입은 두꺼운 식물 도감을 읽기 시작했고,\n그러는 동안 시간은 흘러 2048년이 되었다."
    entertoContinue
    printer 8 "퀸 오브 파이터즈 48은 정식출시되었지만 관리자는 여전히 프로토타입에게 붙잡혀 식물 도감을 읽고 있다고 한다."
    entertoContinue

    mapM_ putStrLn [ "    ·····················"
                    ,"    :+-+-+-+-+-+ +-+-+-+:"
                    ,"    :|P|L|A|N|T| |E|N|D|:"
                    ,"    :+-+-+-+-+-+ +-+-+-+:"
                    ,"    ·····················" ]

    putStr "0: 다시 한다."
    putStr "\n선택: "

    select [startGame]

scenePrototype2_1 ::  [Int] -> String ->  IO ()
scenePrototype2_1 ints player = do
    prototypeSmall
    printer 10 "프로토타입: 그럼 나가세요. 저 자야 하니까."
    entertoContinue
    printer 10 "당신은 시무룩해진 채로 쫒겨났다."
    printer 10 "어디로 가시겠습니까?"
    delayDisplayStr "0. 프로토타입 방\n1. 윤리위원회 회의실\n2. 방송실"

    superSelect (0 : ints) player [scenePrototype (0:ints) player, sceneJeong (0:ints) player, sceneBroad0 (0:ints) player]

scenePrototype3 :: String ->  IO ()
scenePrototype3 player = do
    prototypeDead
    printer 5 $ player ++ "의 뿅망치에 맞아서 프로토타입이 죽었습니다.."
    printer 5 "전부 당신 탓입니다."
    entertoContinue
    printer 5 "다시 하시겠습니까?"
    printer 5 "0: 다시 한다."
    putStr "\n선택: "

    select [startGame]

sceneJeong :: [Int] -> String ->  IO ()
sceneJeong ints player = do
    printer 10 "적막이 흐른다.."
    entertoContinue
    printer 5 $ player ++ ": 여긴 아무도 없나?"
    entertoContinue

    jeong
    printer 10 "정규리: 관리자님."
    printer 5 "정규리: 윤리위원회 회칙 073. 회의 중일 때 회의 참여 인원이 아닌 사람은 회의실에 들어올 수 없다.에 의해 \n관리자님은 지금 나가셔야 합니다."
    entertoContinue

    printer 5 $ player ++ ": 하지만 지금은 회의 중이 아니잖아."
    printer 5 "정규리: 저 혼자 회의 중입니다."
    entertoContinue
    jeong2
    printer 5 "정규리가 당신을 째려본다."

    delayDisplayStr "0. 도망간다. \n1. 같이 회의하자고 한다,"
    putStr "\n선택: "

    select [sceneJeong0 ints player, sceneJeong1 ints player]

sceneJeong0 :: [Int] -> String ->  IO ()
sceneJeong0 ints player = do
    printer 10 "당신은 불쌍해진 채로 쫒겨났다.."
    printer 10 "어디로 가시겠습니까?"
    delayDisplayStr "0. 프로토타입 방\n1. 윤리위원회 회의실\n2. 방송실"
    putStr "\n선택: "

    superSelect (1:ints) player [scenePrototype (1:ints) player, sceneJeong (1:ints) player, sceneBroad0 (1:ints) player]



sceneJeong1 :: [Int] -> String ->  IO ()
sceneJeong1 ints player = do
    printer 3 "정규리: 윤리위원회 회칙 001. 관리자는 윤리위원회에 간섭할 수 없다. 에 의해 그건 불가능합니다."
    printer 10 "당신은 불쌍해진 채로 시무룩해서 쫒겨났다.."
    printer 10 "어디로 가시겠습니까?"
    delayDisplayStr "0. 프로토타입 방\n1. 윤리위원회 회의실\n2. 방송실"
    putStr "\n선택: "

    superSelect (1:ints) player [scenePrototype (1:ints) player, sceneJeong (1:ints) player, sceneBroad (1:ints) player]



sceneBroad :: [Int] -> String ->  IO ()
sceneBroad ints player = do
    printer 3 $ player ++ ": 방송실이다."
    printer 3 "무엇을 할까?"

    mapM_ delayDisplayStr ["0: 같이 게임하자고 사내 방송을 내보낸다."]
    putStr "\n선택: "

    select [sceneBroad0 ints player]

sceneBroad0 :: [Int] -> String ->  IO ()
sceneBroad0 ints player = do
    printer 3 $ "방송: 지금부터 저 ‘관리자’" ++ player ++ " 와 같이 ‘재미있는 신작’ 게임할 사람은 모두 지하 1층으로 모여 주시길 바랍니다."
    entertoContinue

    printer 20 "........"
    entertoContinue
    mapM_ (printer 5) [player ++ ": 뭐지, 반응이 없는데?"
                                , player ++ ": 한번 더 할까?" ]

    entertoContinue
    printer 25 "문이 열린다.."
    entertoContinue
    seol
    mapM_ (printer 5) ["남설: 관리자!"
                        ,"남설: 도대체 이 시간에 뭐하는 거야? 지금 새벽 2시라고." ]
    entertoContinue
    mapM_ (printer 5) [player ++ ": 뭐?"
                        ,player ++ ": 근데 설이는 왜 아직 여기에.." ]
    entertoContinue

    mapM_ (printer 5) ["남설: 책 읽느라 차를 놓쳤어."
                        , "남설: 아무튼, 책 읽는 데 방해되잖아!"
                        , "남설이 당신을 뽕망치로 때리려고 한다." ]
    printer 2 "0. 사과한다."
    printer 2 "1. 사과하지 않는다."
    putStr "\n선택: "

    select [seol0 ints player, seol1 player]

seol0 :: [Int] -> String ->  IO ()
seol0 ints  player = do
    mapM_ (printer 5) [player ++ ": 미안! 미안해!!"
                                , "남설: 이번만 봐 줄게.."
                                , "남설: 들어가서 잠이나 자." ]
    printer 10 "당신은 시무룩불쌍새끼고양이가 되어서 쫒겨났다.."
    printer 10 "어디로 가시겠습니까?"
    delayDisplayStr "0. 프로토타입 방\n1. 윤리위원회 회의실\n2. 방송실"
    putStr "\n선택: "
    superSelect (2:ints) player [scenePrototype (2:ints) player, sceneJeong (2:ints) player, sceneBroad0 (2:ints) player]


seol1 :: String ->  IO ()
seol1 player = do
    printer 5 $ "남설의 뿅망치에 맞아서 "++player ++ "가 죽었습니다."
    printer 5 "전부 당신 탓입니다."
    entertoContinue
    printer 5 "다시 하시겠습니까?"
    printer 5 "0: 다시 한다."
    putStr "\n선택: "
    select [startGame]



sceneFinal :: String -> IO ()
sceneFinal player = do
    printer 10 "방으로 돌아온 당신."

    printer 10 $ player ++ ": 하.. "
    entertoContinue

    se
    printer 5 "비서 로봇: 관리자님, 혼자시네요?"
    entertoContinue
    printer 10 $ player ++ ": 아무도 나랑 게임 안 한대.. "
    printer 5 "비서 로봇: 그러니까 평소에 잘하셨어야죠."
    entertoContinue
    printer 10 $ player ++ ": 너무 슬프다.."
    printer 5 "비서 로봇: 어휴.."
    printer 5 "비서 로봇: 저랑 같이 하실래요?"

    putStr "0: 그래. \n: 아니."
    putStr "\n선택: "

    select [sceneFinal0 player, youDie player]



sceneFinal0 ::  String -> IO ()
sceneFinal0 player = do
    mapM_ (printer 10)  [player ++ ": 정말?"
                , player ++ ": 그럼 어쩔 수 없지.."
                , player ++ ": 한 판 붙자."]
    entertoContinue
    mapM_ (printer 10)  ["비서 로봇: (기기에 연결 중…)"
                        , "비서 로봇: (연결 완료)"
                        , "비서 로봇: 관리자님, 준비되셨나요?"]

    entertoContinue
    seLoved
    mapM_ (printer 10)  [player ++ ": 준비됐어!"
                , "게임 시작.."]
    entertoContinue
    mapM_ putStrLn ["·····················"
        ,":+-+-+-+-+-+ +-+-+-+:"
        ,":|H|A|P|P|Y| |E|N|D|:"
        ,":+-+-+-+-+-+ +-+-+-+:"
        ,"·····················"]

    mapM_ (printer 10) [ "그렇게 당신은 비서 로봇과 행복하게 게임을 즐겼다."
                        ,"아무리 강한 당신일지라도 - 콘솔에 직접 연결된 비서 로봇을 이기는 일은 일어나지 않았지만"
                        ,"    당신은 오랜만에 ‘패배’라는 것을 해 보며, 평소와는 다른 즐거움을 느꼈다."
                        ,"    그렇게 2048년.."
                        ,"    퀸 오브 파이터즈 48은 정식 출시되었으나"
                        ,"    비서 로봇과의 대전으로 단련된 당신의 실력을 따라올 인간은 아무도 없었고"
                        ,"    당신은 더 많은 시간을 비서 로봇과 게임을 하며 보내게 되었다."
                        ,"    그렇게 사랑은 싹트고.."
                        ,"    그렇다."
                        ,"    그렇게 된 것이다."]

    threadDelay 100000
    printer 5 "끝을 보았습니다. 원한다면 0을 눌러 처음으로 돌아가세요.."
    putStr "\n선택: "

    printer 5 "0: 다시 한다."
    select [startGame]


youDie :: String ->  IO ()
youDie player = do
    printer 5 $ "게임을 하지 못한 "++player ++ "는 말라 죽었습니다."
    printer 5 "전부 당신 탓입니다."
    entertoContinue
    printer 5 "다시 하시겠습니까?"
    printer 5 "0: 다시 한다."
    putStr "\n선택: "

    select [startGame]

------ASCII------------





seLoved :: IO ()
seLoved = mapM_ putStrLn [  "                                            █ ██                       "
                            ,"                    ███████████                      ███████████       "
                            ,"        ██████████                                              █      "
                            ,"    █                                                                  "
                            ,"                                                                 █     "
                            ,"   █                                                                   "
                            ,"   █                                                      ██      █    "
                            ,"   █                                                    ██████    █    "
                            ,"   █                             ██ ███                 ██████         "
                            ,"   █                             ██████                  ███       █   "
                            ,"   █     ███████                 ██ ███                            █   "
                            ,"   █                             ██████                                "
                            ,"   █                             █████                                 "
                            ,"   ██                                                                  "
                            ,"   ███                                                              █  "
                            ,"   ██████                                                    ███████   "
                            ,"     ███████████████████████████████████████████████████████           "
                            ,"                                                                       "
                            ,"                                                                       "
                            ,"                                                                       "
                            ,"                                                                       "
                            ,"                                                            █   █████  "
                            ,"                                                         ████████    █ "
                            ,"                                                         █           █ "
                            ,"                                                         █           █ "
                            ,"                                                          █         █  "
                            ,"                                                            ██    █    "
                            ,"                                                              ███      " ]

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
seDie = mapM_ putStrLn [      "                          ████████████████████                         "
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

prototypeDark :: IO ()
prototypeDark = mapM_ putStrLn [
                            " ██████████ ████████████████████ ████ ███ ███████████████ ██ █████████ "
                            ," ██████████ ██ ████████████████ █ ███████ ████████████████ ███████████ "
                            ," ██████████ ██ ████████████████ █ ████████ ███████████████ █ ████████  "
                            ," ██████████ ███ ████████████████ █ ██ █████ ██████     ███ █  ██ ██ ██ "
                            ," ██████████ ████████████████████ ██ ████ ███ █████████████  █  ███████ "
                            ," ██████████████  ████████████████ ██ ███████  ████████████ ██    ███ █ "
                            ," ███████   ██████████████████████ ██ █████  █ ████████████ ███  ███  █ "
                            ," ████  ███ ███████████████████████ ██ ████████ ███████████  ██   ██ ██ "
                            ," █████ ███ ███████████████████████ ███ █ ██████ ███████████ ███ ███ ██ "
                            ," █████ ███ ████████████████████████ ████ ███  ██ ██████████ ███ ███  ██"
                            ," █████ ███ ████      ███ ███████████ ███████  █   █████████ ███ █████  "
                            ," █████ ███            ██   ██████████ █████████             ███ ██  ██ "
                            ," ██████ ██             ████   █████████████████             ███ ██ ███ "
                            ,"█ ██ ██ ███            █  ██████████████████████           ██   ███ ██ "
                            ,"  ███   █ ██          ████ ████████████████████████       █████ ██  ██ "
                            ," █ ██ █ █ ███       ████████████████████████████████████████ ██ ██ ██  "
                            ," █ ███  █ █████████████ ███████████████████████              ██ ██ ███ "
                            ," ██ ██ ████   █████    █████████████████████████████   ████████ ██  ██ "
                            ," ██ ████ ██████      ██████████████████████████████████████████ ██ ███ "
                            ," ███ ██  ██████████████████████████ ███████████████████████████ ██     "
                            ," ███ ███ ██████████████████████████████████████████████████████ ██ ███ "
                            ," ████ ██ ██████████████████████████████████████████████████████ ██ █ █ "
                            ," ████ ████████████████████████████████████████████████████████  ██ █   "
                            ," ████████ ███████████████████████████████████████████████████ █ ██ █   "
                            ," █████ █████████████████████████████████████████████████████ ██ ██ ███ "
                            ,"   ███ ███ ████████████████████████████████████████████████  ██ █  ██  "
                            ," ███ ██ ███ █████████████████████████████████████████████  ██ █ █ ██ █ "
                            ," ████ █ ███████████████████████████████████████████████   ██  █ █ █ ██ "
                            ," █████ ██ ██ ███████████████████████████████████████    █ █  ██ █ ████ "
                            ," ██████  █ ██ ████████████████████████████████████ ██         █  █████ "
                            ," ███████ ██ ██ ██ █████████████████████████████ ████████ ████ █  █████ "
                            ," ███████ ███  ███ ██████  ██████████████████ ███████ ██ ███████ █   ██ "
                            ," ████████ ██ ██ █ ███████ ████   ███████████████████ █████████  ██  ██ "
                            ," █      █ ██              ██████████████████████████ █ █     █ ██     " ]

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

prototypeSmall :: IO ()
prototypeSmall = mapM_ putStrLn [ "    ███████ ██  ████████  █████████████ ███   ██████ ██     █   ██ ███    "
                        ,"   █████ ███ ████████████   █     ██  █████████████████████████ ██ ███ "
                        ," █ █████ ███ ████████   █ ██ █████     ██████               ███ ██  ██ "
                        ," █  ██ █  █            █  ██       █████████████            █ █  █ ███ "
                        ," ██ ██ ██ █          █████ ██████████████████████          █  █ ██ ███ "
                        ," ██  ██ █ █          ██████ █████████████████████          █  █  █   █ "
                        ," ███ ██ █ █         ████████████████████████████████   █████ ██  █ ██  "
                        ," █ █  ██  ███     █████████████████████████████  ████████    ██ ██ ███ "
                        ," ████ ██ █ ██████████  █████████████████████████          █████ ██ ███ "
                        ," ████ ███            ███████████████ ██████████████████████████  █ ███ "
                        ," █████ ██  ████████████████████████████████████████████████████  █     "
                        ," █████ ██  ████████████████████████████████████████████████████ ██ █   "
                        ," ██████ ██ ███████████████████████████████████████████████████  ██     "
                        ," ██████ ██  █████████████████████████████████████████████████   ██     "
                        ,"  ██████ █  ████████████████████████████████████████████████    ██     "
                        ,"    ████ ██  ██████████████████████████████████████████████   █ █  █   "
                        ,"   █   ██ ██ ████████████████████          ███████████████      █      "
                        ,"   ████ █ ██   ██████████████████ ██████████████████████        █      "
                        ,"                    █ ████████████████████████                         "]

prototypeSmile :: IO ()
prototypeSmile = mapM_ putStrLn [" █████████████  ████████████████ █████████████████████████████████████ "
                                ," █████████████  █ ██████████████ ███████████████████████ █████████████ "
                                ," ████████   █████████████████████ ███████ ████████████████████████████ "
                                ," ██████ ██████    ███████████████  █████████████████████ █████████████ "
                                ," ██████ █   ██     ███████████████  ███████ █████████████ ████ ███████ "
                                ," ██████ █ ████     ███████████████   █       █████████████████ █ █████ "
                                ," ██████ █████       ████  █████████   ███ ██  ████████████ ███████ ███ "
                                ," ██████ ██ ██        ███████████████     █     ███████████ ████  ███  █"
                                ," ███████ █ ██          █  ███████████ ██     ██  █      █  █████    █  "
                                ," █ ██ ██ █ ██ ████████ ███ ███████    ███ █████████████ █████ █     █  "
                                ," █████ █ █ █████████████ ██                          █████ ██ ██ █ ██ █"
                                ," █████ ███████             █                               ██ █ ██ █  █"
                                ," ███ ██ ███                                     █  █     █ ██ █ █ ██ █ "
                                ," ██████ █ ██       █                        █  █ ████  █ █ ██ █     ██ "
                                ," ███████ ███ █ ██ █  ██                       █  ███  ██ █ ██ █    ███ "
                                ," ████ ██ ███ ███ ██ ██            ██         █  ████ ██  █ ██ ████████ "
                                ," █████ █  █   █  █  █                           █          ██ ████████ "
                                ,"  ████ ██ █                                                ██ ████████ "
                                ," █  ███ █ ██                                              ██  ████████ "
                                ," ██   █ █ ███                                            ███  ████████ "
                                ," █████   █ ██                  █        █               ████ █████████ "
                                ," ███ ███ █ ██                 ███████████             ██████ █████ ███ "
                                ," ███   ████ ████               ██████████           ████████ █████████ "
                                ," ███ ██ █████ ████               █████           █████████████████████ "
                                ," ███ ███ ██ ██ ██████                         ███ ████████████████████ "
                                ," ███  ███ ██ ██ ████████                   ███    █ ██  ██████████████ "
                                ," ███ ████  █  ████████████████          ███      ██       ████████████ "
                                ," ███  ████ █ ██████████████ ████████████     █   ████ █   ██ █████████ "]

prototypeEnd :: IO ()
prototypeEnd = mapM_ putStrLn ["  █████      ███████████████████ █   █  █████████ ████████████ ███ ███ "
                                ,"  ██ ██       █  ███████████████  █   █ █████    █████████████ ████ ██ "
                                ,"  ██ █         ██ ███████████████ █    █          █ ███████████████ ██ "
                                ,"  ██ █          █  ██   █████████  █    ███        █  ███████████ ████ "
                                ," █ █ █           █  █ █ ██████████  █        ████   █  ██████   █ ██ █ "
                                ," █ █ █████        █   █ ███████████ ██        ████████        █ █ ██ █ "
                                ," █ █ █  ████████████  ██ █    █████████          ████████████████ ██ █ "
                                ," █   █████████████████ █  ███████                ███████████████████ █ "
                                ," ██  ███████████████  ██                          ██████████████     █ "
                                ,"  █  ██████████████     █                          ████████████      █ "
                                ,"  █ ███████████████      █                           ██████       █  █ "
                                ,"  █ █    █████                                                 ████  █ "
                                ,"  █ █               █                            ████████████████    █ "
                                ," ██ █████     ███████                                       █ █      █ "
                                ," ███    ███████                                      ████████████    █ "
                                ,"  ██               ███             █                                 █ "
                                ,"                                                                     █ "]

prototypeDead :: IO ()
prototypeDead = mapM_ putStrLn [    "                █                                                      "
                                    ,"                █                                                      "
                                    ,"                ██                                                     "
                                    ,"                ███                █                                   "
                                    ,"                  ██               █                                   "
                                    ,"               ██████               █                                  "
                                    ,"             █ ███████               █                                 "
                                    ,"               ████████               ███                              "
                                    ,"             █ ██████    █               ███                           "
                                    ,"                           ██        ███████  ███                      "
                                    ,"              ███        ██ █████████████████             █            "
                                    ,"              ███        █████████████████████   █       ██            "
                                    ,"                 ██     ██████████████████████████████████             "
                                    ,"              █     █████████████████████████                   █      "
                                    ,"              ████████   ███████████████████████████  █████            "
                                    ,"                 █████████████████████████████████████████             "
                                    ,"             ████████████████████████████████████████████              "
                                    ,"             ████████████████████████████████████████████ █            "
                                    ,"              ████████████████████████████████████████████             "
                                    ,"              ███████████████████████████████████████████              "
                                    ,"               █████████████████████████████████████████               "
                                    ,"                ██████████████████     ███████████████                 "
                                    ,"                  ██████████████████████████████████                   "
                                    ,"                    █████████████████████████████                      "
                                    ,"      █                ████████████████████████                        "
                                    ,"      █                    █████████████████   ███                     "
                                    ,"      █        █            ██  ████████████████ █                     "
                                    ,"      █                     ██████████████████   █                     "
                                    ,"      █                     █████████████████    █                     "
                                    ,"                          ████████████████████                  █      "
                                    ,"                ████████████████████████████████               ████████"]

jeong:: IO ()
jeong = mapM_ putStrLn ["             █      █                                                  "
                        ,"                                                                       "
                        ,"              ██                                                       "
                        ,"                      ██                █                              "
                        ,"                        █                ██                            "
                        ,"               ███      ██                                             "
                        ,"                                           ██                          "
                        ,"                            █  ████████ █     ███                    █ "
                        ,"                        ██████ ██        █ ███████        █            "
                        ,"           █            ██████ ████████████████  ████████   █          "
                        ,"          █████        ██  ███ █████████████ ███████████████           "
                        ,"           ██  ████████  ████ ███████████████                          "
                        ,"            ██████ ████████ ███████████████████████████████            "
                        ,"             ███████      ████████████ ███████████████████           █ "
                        ,"             █████████████████████████  ██████████████████        ████ "
                        ," ████         ████████████████████████  ███████████████████            "
                        ,"   ████       ████████████████████████████████████████████             "
                        ,"      █        ███████████████████████████████████████████             "
                        ,"                ██████████████████████████████████████████             "
                        ,"                 ██████████████████████████████████████████            "
                        ," █                ██████████████████          █████████████            "
                        ,"  ███              ██████████████████████████████████████              "
                        ,"      ███  ███      █████████████████████████████████                  "
                        ,"              ███     █████████████████████████████    █               "
                        ,"                        ████████████████████████      ██               "
                        ,"                                                                       "]

jeong2 :: IO ()
jeong2 = mapM_ putStrLn [" ████████████ ████████████████████████████████████████████████████████ "
                        ," ████████████ ██████ █████████████████████████████████████████████████ "
                        ," █████████████████████████████████████████████████████████████████████ "
                        ," ██████████ ██  ██████████████████████████████████████████████████████ "
                        ," █████████████ ██████   █ ██████████████ █████████████████████████████ "
                        ," ███████████████ ████ █  ████████████████  ██████████  ███████████████ "
                        ," ██████████████   ███  █  ██  █████████████████████████████████ ██████ "
                        ," ████████████████████████ █████████████████  █████████████████████████ "
                        ," ██  ████████ ██████████████  █          █████   ████████████████████  "
                        ," ██  ███████████████████      █    ██████ █       ████████ ███████████ "
                        ," █████████  ████████████      █                 █        ███ █████████ "
                        ," █████ ██      ███████    █                 █               ██████████ "
                        ," ██████████  ██         █    █               █████  ██████████████████ "
                        ,"  █████████                █                                ██████████ "
                        ,"   ██████████        █████                                 ██████████  "
                        ,"  ████████ ██                         █                     ██████   █ "
                        ,"      ███████                         █                     ██████████ "
                        ," ██    █ █████                                             ███████████ "
                        ," █████ ███ ████                                            ███████████ "
                        ," ██████████ ████                                          █ ██████████ "
                        ," ████████████ ███                                          ████████████"
                        ,"   ████████████ ██                    ██████               ████████████"
                        ,"      ██████████ ██                                      ██████████████"
                        ,"█████          █████                                  █████████████████"
                        ,"█████████ ████    █                                 ███ ███████████████"
                        ,"████████████████████████                         █████  ███████████████"
                        ," ██████████████████████████                              █████████████ "]

seol :: IO ()
seol = mapM_ putStrLn  [" ██████████████████████████████████████████████████████████████ █ ████ "
                            ," ██████████████████████████████████████████████████████████████ █ ████ "
                            ," █████████████████████████████████████████████████████████████████████ "
                            ," █████████████████████████████████████████████████████████████████████ "
                            ," █████████████████████████████████████████████████████████████████████ "
                            ," ███████████████████████████████████████████████████ █████████████████ "
                            ," ███ █████████████ ███████████ ████████████████████████████  █ ███████ "
                            ," ████ █████████████████████████ █████████████████████  ██████████ ████ "
                            ," █████ ███████████  ██████████ █ █████████████████████ █████ █████████ "
                            ," ███ ██ ██████████████████████ ███ █████████████████████████ ████ ██   "
                            ," ███████   ███████████  ███████ ███  █████████████████ █ ██████████████"
                            ," ████  ██ █████████████████████ ██████████████████ ██   █   ██ █████   "
                            ," █████ ███████████████ ██  ██████████ █████████   ████████████ ███████ "
                            ," █████████  █   ██████   ███████████          █ ████       ███ ███████ "
                            ," █████████ █ ███     ██████   █               ██  █████████ ██████████ "
                            ," ██████████ ██ ██████████  ██                 █  ████████████████████  "
                            ," ███████ ████████████████                        ████████████████████  "
                            ," ███████████ ████████████                         ████████████████ █   "
                            ,"   ██████ █  ███████████                             ██████  ███████   "
                            ," ████████ █     ██████                                       ███████   "
                            ,"  █ █████ █                                         ███████████████  █ "
                            ,"  █████████   ██████████                                    ███████    "
                            ,"   █████████                         █                      ████████   "
                            ," █████████ █                         █                      ███████    "
                            ,"    █ ████ █                                                ██████  ██ "
                            ,"    ██████ █                                                █ ████████ "
                            ," ██  █ ███ █                                                █ ███ ████ "
                            ," █████ █████                                                █ ███ ████ "
                            ,"     ██ ███ █                                               ████ █████ "
                            ," ██   █ ███   █                       ████                 █████ █████ "
                            ," ██████  ██ ███ █                   ███████               ████████████ "
                            ,"██     █ ██ ███████                  ██                 ██████████████ "
                            ,"    █████   ██████████                                █████████████    "
                            ," ████████   █████████████                          ██████  █  ███  ███ "
                            ," █████████  ████████████████                    ██         █    ██████ "
                            ," ██████████  ██████████████  █████          ███      █  █ ██ █     ███ "
                            ," ███████████ ███████  ██████      █████████           █ █ ███          "
                            ," ██████████ ██     ████   ██                          █ █  ██  █       "
                            ," ███████   █  ██████   ███ █                        █       █   █      "
                            ," ██ ███████████████████   █                       █   █      █  █   ██ "
                            ]