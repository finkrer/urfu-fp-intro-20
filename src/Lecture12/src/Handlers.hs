{-
  Этот модуль содержит функции для обработки API запросов.
  В MVC паттерне их можно рассматривать как контроллеры.
-}
module Handlers where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Servant.Server

import App
import Data.Time
import DB.MovieSession
import DB.Seat (Seat, SeatId, getSeatsBySessionId)
import DB.Preliminary
import DB.Booking
import Utils

getSessions :: MonadIO m => AppT m [MovieSession]
getSessions = getMovieSessions

getSeats :: MonadIO m => MovieSessionId -> AppT m [Seat]
getSeats = getSeatsBySessionId

postPreliminary :: MonadIO m => MovieSessionId -> SeatId -> AppT m BookingId
postPreliminary msId seatId = do
  bookings <- createPreliminary msId seatId
  case bookings of
    (b:_) -> pure $ bookingId b
    _ -> throwJSONError err404 $ JSONError "booking is not found"

bookingDuration :: NominalDiffTime
bookingDuration = 600

checkoutBooking :: MonadIO m => BookingId -> AppT m String
checkoutBooking bId = 
  getBookings bId >>= \case
    [] -> return "Checkout failed"
    booking:_ ->
      if (not $ isPreliminary booking)
        then return "Booking was already checked out"
        else do
          delete booking
          let bookingTime = createdAt booking
          currentTime <- liftIO $ getCurrentTime
          if diffUTCTime currentTime bookingTime >= bookingDuration
            then return "Booking is more than 10 minutes old and has expired"
            else checkout booking
              >> return "Booking checked out successfully"

refundBooking :: MonadIO m => BookingId -> AppT m String
refundBooking bId = 
  getBookings bId >>= \case
    [] -> return "Refund failed"
    booking:_ -> delete booking >> return "Refund succeeded"