module MonadMyStateT where

newtype MyStateT s m a = MyStateT { runMyStateT :: s -> m (a, s) }

{- instance Monad m => Monad (ReaderT r m) where
    (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
    m >>= k = ReaderT $ \e -> do 
--                            🠕
--              вычисление во внутренней монаде
        v <- runReaderT m e
        runReaderT (k v) e
-- TrRW.runReaderT (do {x <- listRead; return ((+1) x)}) 4

 -}

instance Monad m => Monad (MyStateT s m) where
--  (>>=) :: (MyState s m a) -> (\a -> MyState s m b) -> MyState s m b  
--                 m                      k
    m >>= k = MyStateT $ \st -> do
                                    (v, st') <- runMyStateT st  -- (v, st') <=> (a, s)
                                    runMyStateT (k v) st'



{- Реализация функции fail
Протаскивание ошибки без дополнительной обработки

instance (Monoid w, MonadFail m) => MonadFail (WriterT w m) where
    fail :: String -> WriterT w m a
    fail = WriterT . fail
 -}

instance MonadFail m => MonadFail (MyState s m) where
    fail = MyStateT . fail



{- Протаскивание внутренней монады наружу
instance (Monoid w) => MonadTrans (WriterT w) where
    lift :: Monad m => m a -> WriterT w m a
    lift m = WriterT $ do
        x <- m
        return (x, mempty)

listWrite = WriterT $ [(1,"one"),(2,"two"),(3,"three")]
{-
TrRW.runWriterT $ do {x <- listWrite; f <- TrRW.lift [(+4),(*5)]; return (f x)} -}

-}

instance MonadTrans (MyStateT s) where
--    lift :: Monad m => m a -> MyStateT s m a 
    lift m = MyStateT $ \st -> do {v <- m; return (v, st);}

