module P16.TicketNotesParser(
  parseTicketNotes
  ) where

import Text.ParserCombinators.Parsec(
  Parser,
  ParseError,
  char,
  digit,
  endBy,
  lower,
  many,
  many1,
  manyTill,
  newline,
  try,
  sepBy,
  string,
  space,
  parse,
  (<|>)
  )

import P16.TicketNotes(
  TicketNotes(..),
  FieldPair,
  FieldRanges,
  FieldRange,
  Ticket
  )

parseTicketNotes :: String -> Either ParseError TicketNotes
parseTicketNotes = parse ticketNotes ""

ticketNotes :: Parser TicketNotes
ticketNotes = do
  ticketSchema <- manyTill ticketField (try newline)
  string "your ticket:" >> newline
  yours <- ticketValues
  emptyLine
  string "nearby tickets:" >> newline
  nearbys <- ticketValues `endBy` newline
  return TicketNotes{ schema = ticketSchema, yourTicket = yours, nearbyTickets = nearbys }

emptyLine = string "\n\n"

ticketField :: Parser FieldPair
ticketField = do
  fieldName <- ticketFieldName
  string ": "
  ranges <- ticketFieldRanges
  newline
  return (fieldName, ranges)

ticketField' :: Parser FieldPair
ticketField' = (,) <$> (ticketFieldName <* string ": ") <*> ticketFieldRanges'

ticketFieldName :: Parser String
ticketFieldName = many (lower <|> space)

ticketFieldRanges :: Parser FieldRanges
ticketFieldRanges = do
  firstRange <- ticketFieldRange
  string " or "
  secondRange <- ticketFieldRange
  return (firstRange, secondRange)

ticketFieldRanges' :: Parser FieldRanges
ticketFieldRanges' = (,) <$> (ticketFieldRange' <* string " or ") <*> ticketFieldRange'

ticketFieldRange :: Parser FieldRange
ticketFieldRange = do
  low <- read <$> many1 digit
  char '-'
  high <- read <$> many1 digit
  return (low, high)

ticketFieldRange' :: Parser FieldRange
ticketFieldRange' = (,) <$> (read <$> many1 digit <* char '-') <*> (read <$> many1 digit)

ticketValues :: Parser Ticket
ticketValues = map read <$> many1 digit `sepBy` char ','
