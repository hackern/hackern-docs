{-
    A Formal Description of Our Computation Model
-}

-- import Distributed.HackernAPIs (words, map, reduceByKey)
import Prelude hiding (words, map)

data DataConnection a = DataConnection {
    dataRecvPort  :: ReceivePort a,
    dataURLhandle :: URL -> Process ()
}

connect :: NodeID -> Task DataConnection
connect nodeId = do
    self <- liftIO $ getSelfPid
    dataServerPid <- spawn nodeId dataServer
    (sp, rp) <- newChan :: Process (SendPort Text, ReceivePort Text)
    send dataServerPid sp
    return DataConnection {
        dataRecvPort  = rp,
        dataURLhandle = \url -> send dataServerPid url
    }

getTextByURL :: DataConnection -> URL -> Task Text
getTextByURL con url = do
    dataURLhandle con url
    text <- receiveChan port
    return text

H.map :: (a -> (a, b)) -> [a] -> Task [(Mapper a b)]
H.map f as = do
    workers <- getWorkers
    mappers <- runWorker workers $ SimpleSplit as f
    return mappers

H.reduceByKey :: (b -> b -> b) -> [Mapper a b] -> Task (Map a b)
H.reduceByKey reduceFn mappers = do
    results <- mapM_ waitMapper mappers :: Map a b
    return $ unionsWith reduceFn results

wordCount :: NodeID -> Task ()
wordCount dataNodeId = do
    con <- connect dataNodeId
    text <- getTextByURL con $ URL "examples" </> "hello_world.txt"
    mapped <- H.map (\w -> (w, 1)) $ H.words text
    reduced <- reduceByKey sum mapped
    liftIO $ print reduced


