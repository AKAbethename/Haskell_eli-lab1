{-# LANGUAGE InstanceSigs #-}

module MonadMyStateT where

newtype MyStateT s m a = MyStateT { runMyStateT :: s -> m (a, s) }


{- -- Функтор
instance Functor (Writer w) where
    fmap :: (a -> b) -> Writer w a -> Writer w b
    fmap f = Writer . update . runWriter
        where update ~(y, log) = (f y, log)
{-                   🠕
            неопровержимый образец
    (сопоставление происходит только тогда если 
        вычисления нужны в правой части вычисления.
        Т.е. это ленивая версия Writer, если убрать ~ то будет строгая версия)
-}

instance Functor m => Functor (WriterT w m) where
    fmap :: (a -> b) -> WriterT w m a -> WriterT w m b
    fmap f = WriterT . fmap update . runWriterT
        where update ~(y, log) = (f y, log)
 -}

{-  instance Functor m => Functor (ReaderT r m) where
    fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
    fmap f x = ReaderT $ fmap f . runReaderT x -}

instance Functor m => Functor (MyStateT s m) where
    fmap :: (a -> b) -> MyStateT s m a -> MyStateT s m b
--                 f                 x             
    {- fmap f x = MyStateT $ \st -> do
                                    (v, st') <- runMyStateT x st 
                                    return (f v, st') -}

    fmap f x = MyStateT $ fmap update . runMyStateT x 
        where update ~(y, st) = (f y, st)



{- 
Аппликативный функтор
instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
    pure :: a -> WriterT w m a
    pure x = WriterT $ pure (x, mempty)
    (<*>) :: WriterT w m (a -> b) -> WriterT w m a -> WriterT w m b
    f <*> v = WriterT $ liftA2 update (runWriterT f) (runWriterT v)
        where update ~(g,w) ~(x,w') = (g x, w `mappend` w') 
        -}


{- instance Applicative m => Applicative (ReaderT r m) where
    pure :: a -> ReaderT r m a
    pure = ReaderT . const . pure
--                            🠕
--      нужно поднять оборачиваемое значение в контекст монады
    (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
    f <*> v = ReaderT $ \ e -> runReaderT f e <*> runReaderT v e
{-                         :t runReaderT f e  :t runReaderT v e -}

instance Monoid w => Applicative (Writer w) where
    pure :: a -> Writer w a
    pure x = Writer (x, mempty)
    (<*>) :: Writer w (a -> b) -> Writer w a -> Writer w b
    f <*> v = Writer $ update (runWriter f) (runWriter v)
        where update ~(g,w) ~(x,w') = (g x, w `mappend` w')

instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
    pure :: a -> WriterT w m a
    pure x = WriterT $ pure (x, mempty)
    (<*>) :: WriterT w m (a -> b) -> WriterT w m a -> WriterT w m b
    f <*> v = WriterT $ liftA2 update (runWriterT f) (runWriterT v)
        where update ~(g,w) ~(x,w') = (g x, w `mappend` w')

-}


{- instance Applicative m => Applicative (MyStateT s m) where
    pure :: a -> MyStateT s m a
    pure x = MyStateT $ \st -> pure (x, st) 
    (<*>) :: MyStateT s m (a -> b) -> MyStateT s m a -> MyStateT s m b
--                        f                      x
--    f <*> x = MyStateT $ \st -> (runMyStateT f st) <*> (runMyStateT x st)
    f <*> x = MyStateT $ \st -> liftA2 update (runMyStateT f st) (runMyStateT x st)
                                    where update ~(g, s') ~(y, s'') = (g y, s') -}


instance (Applicative m, Monad m) => Applicative (MyStateT s m) where
    -- pure :: a -> MyStateT s m a
    pure x = MyStateT $ \st -> return (x, st)

    -- (<*>) :: MyStateT s m (a -> b) -> MyStateT s m a -> MyStateT s m b
    --                      f                      x 
    f <*> x = MyStateT $ \st -> do
                                    ~(g, s') <- runMyStateT f st 
                                    ~(y, s'') <- runMyStateT x s'
                                    return (g y, s')

    



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
                                    (v, st') <- runMyStateT m st  -- (v, st') <=> (a, s)
                                    runMyStateT (k v) st'



{- Реализация функции fail
Протаскивание ошибки без дополнительной обработки

instance (Monoid w, MonadFail m) => MonadFail (WriterT w m) where
    fail :: String -> WriterT w m a
    fail = WriterT . fail
 -}


{- instance MonadFail m => MonadFail (MyStateT s m) where
    fail :: String -> MyStateT s m a
    fail = MyStateT . fail
 не обрабатывает ошибки => не нужно -}



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

class MonadTrans t where -- Control.Monad.Trans.Class
    lift :: Monad m => m a -> t m a

instance MonadTrans (MyStateT s) where
--    lift :: Monad m => m a -> MyStateT s m a 
    lift m = MyStateT $ \st -> do {v <- m; return (v, st);}


get :: Monad m => MyStateT s m s
get = MyStateT $ \s -> return (s, s)

put :: Monad m => s -> MyStateT s m ()
put s = MyStateT $ \_ -> return ((), s)

modify :: Monad m => (s -> s) -> MyStateT s m ()
modify f = MyStateT $ \s -> return ((), f s)

