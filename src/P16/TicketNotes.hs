module P16.TicketNotes where

type FieldRange = (Int, Int)
type FieldRanges = (FieldRange, FieldRange)
type FieldPair = (String, FieldRanges)
type TicketSchema = [FieldPair]
type Ticket = [Int]

data TicketNotes = TicketNotes {
  schema :: TicketSchema,
  yourTicket :: Ticket,
  nearbyTickets :: [Ticket]
} deriving Show
