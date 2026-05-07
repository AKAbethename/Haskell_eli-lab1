{-# LANGUAGE InstanceSigs #-}

module MonadMyStateT where

import Control.Monad.IO.Class (MonadIO(..))


newtype MyStateT s m a = MyStateT { runMyStateT :: s -> m (a, s) }



instance Functor m => Functor (MyStateT s m) where
    fmap :: (a -> b) -> MyStateT s m a -> MyStateT s m b
--                 f                 x             
    {- fmap f x = MyStateT $ \st -> do
                                    (v, st') <- runMyStateT x st 
                                    return (f v, st') -}

    fmap f x = MyStateT $ fmap update . runMyStateT x 
        where update ~(y, st) = (f y, st)



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


instance Monad m => Monad (MyStateT s m) where
--  (>>=) :: (MyState s m a) -> (\a -> MyState s m b) -> MyState s m b  
--                 m                      k
    m >>= k = MyStateT $ \st -> do
                                    (v, st') <- runMyStateT m st  -- (v, st') <=> (a, s)
                                    runMyStateT (k v) st'



{- Реализация функции fail
Протаскивание ошибки без дополнительной обработки
 -}


{- instance MonadFail m => MonadFail (MyStateT s m) where
    fail :: String -> MyStateT s m a
    fail = MyStateT . fail
 не обрабатывает ошибки => не нужно -}



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



instance MonadIO m => MonadIO (MyStateT s m) where
    liftIO io = MyStateT $ \s -> do
        a <- liftIO io
        return (a, s)

