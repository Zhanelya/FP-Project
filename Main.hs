{-# LANGUAGE OverloadedStrings #-}
-- ECS713, Group 2: Jean Emmanuel Yannick Messey-Elouma, Zhanelya Subebayeva, Soham Amrish Trivedi
module Main where 
-- PARSING FROM JSON TO XML
-- I/O imports
import System.Environment (getArgs)
-- Aeson imports
import Data.Aeson
import qualified Data.Text as Text_M
import Data.Functor
import qualified Data.ByteString.Lazy as BSL_M
import Control.Applicative
import Control.Monad
-- import functions for querying parse data
import QueryDataFunctions
-- import data types
import DataTypes

-- SAMPLE JSON INPUT
json_in :: JSONData
json_in = "{\n\t\"customers\":[\n\t\t{\n\t\t\t \"firstName\": \"John\",\n\t\t\t \"lastName\": \"Smith\",\n\t\t\t \"age\": 25,\n\t\t\t \"address\":\n\t\t\t {\n\t\t\t\t \"streetAddress\": \"21 2nd Street\",\n\t\t\t\t \"city\": \"New York\",\n\t\t\t\t \"state\": \"NY\",\n\t\t\t\t \"postalCode\": \"10021\"\n\t\t\t },\n\t\t\t \"phoneNumbers\":\n\t\t\t [\n\t\t\t\t {\n\t\t\t\t   \"num_type\": \"home\",\n\t\t\t\t   \"number\": \"212 555-1234\"\n\t\t\t\t },\n\t\t\t\t {\n\t\t\t\t   \"num_type\": \"fax\",\n\t\t\t\t   \"number\": \"646 555-4567\"\n\t\t\t\t }\n\t\t\t ]\n\t\t},\n\t\t{\n\t\t\t \"firstName\": \"Alex\",\n\t\t\t \"lastName\": \"Davis\",\n\t\t\t \"age\": 30,\n\t\t\t \"address\":\n\t\t\t {\n\t\t\t\t \"streetAddress\": \"2 1st Street\",\n\t\t\t\t \"city\": \"New York\",\n\t\t\t\t \"state\": \"NY\",\n\t\t\t\t \"postalCode\": \"10023\"\n\t\t\t },\n\t\t\t \"phoneNumbers\":\n\t\t\t [\n\t\t\t\t {\n\t\t\t\t   \"num_type\": \"home\",\n\t\t\t\t   \"number\": \"211 777-5432\"\n\t\t\t\t },\n\t\t\t\t {\n\t\t\t\t   \"num_type\": \"fax\",\n\t\t\t\t   \"number\": \"633 523-1234\"\n\t\t\t\t }\n\t\t\t ]\n\t\t},\n\t\t{\n\t\t\t \"firstName\": \"Richard\",\n\t\t\t \"lastName\": \"Holmes\",\n\t\t\t \"age\": 23,\n\t\t\t \"address\":\n\t\t\t {\n\t\t\t\t \"streetAddress\": \"22 2nd Street\",\n\t\t\t\t \"city\": \"New York\",\n\t\t\t\t \"state\": \"NY\",\n\t\t\t\t \"postalCode\": \"10021\"\n\t\t\t },\n\t\t\t \"phoneNumbers\":\n\t\t\t [\n\t\t\t\t {\n\t\t\t\t   \"num_type\": \"home\",\n\t\t\t\t   \"number\": \"233 523-1544\"\n\t\t\t\t },\n\t\t\t\t {\n\t\t\t\t   \"num_type\": \"fax\",\n\t\t\t\t   \"number\": \"543 111-1235\"\n\t\t\t\t }\n\t\t\t ]\n\t\t}\n\t]\n}"
{- SRC: http://www.sitepoint.com/customer-form-json-file-example/

	{
		"customers":[
			{
				 "firstName": "John",
				 "lastName": "Smith",
				 "age": 25,
				 "address":
				 {
					 "streetAddress": "21 2nd Street",
					 "city": "New York",
					 "state": "NY",
					 "postalCode": "10021"
				 },
				 "phoneNumbers":
				 [
					 {
					   "num_type": "home",
					   "number": "212 555-1234"
					 },
					 {
					   "num_type": "fax",
					   "number": "646 555-4567"
					 }
				 ]
			},
			{
				 "firstName": "Alex",
				 "lastName": "Davis",
				 "age": 30,
				 "address":
				 {
					 "streetAddress": "2 1st Street",
					 "city": "New York",
					 "state": "NY",
					 "postalCode": "10023"
				 },
				 "phoneNumbers":
				 [
					 {
					   "num_type": "home",
					   "number": "211 777-5432"
					 },
					 {
					   "num_type": "fax",
					   "number": "633 523-1234"
					 }
				 ]
			},
			{
				 "firstName": "Richard",
				 "lastName": "Holmes",
				 "age": 23,
				 "address":
				 {
					 "streetAddress": "22 2nd Street",
					 "city": "New York",
					 "state": "NY",
					 "postalCode": "10021"
				 },
				 "phoneNumbers":
				 [
					 {
					   "num_type": "home",
					   "number": "233 523-1544"
					 },
					 {
					   "num_type": "fax",
					   "number": "543 111-1235"
					 }
				 ]
			}
		]
	}
-}

-- JSON to HASKELL
-- Aeson specific functions:
-- (.:) :: FromJSON a => Object -> Text -> Parser a
instance FromJSON Customers where
	parseJSON (Object v) = Customers 
						  <$> v .: "customers" 
	-- A non-Object value is of the wrong type, so fail.
	parseJSON _ = mzero
	
instance FromJSON Customer where
	parseJSON (Object v) = Customer 
						  <$> v .: "firstName" 
						  <*> v .: "lastName" 
						  <*> v .: "age" 
						  <*> v .: "address"
 						  <*> v .: "phoneNumbers" 
 	-- A non-Object value is of the wrong type, so fail.
	parseJSON _ = mzero
	
instance FromJSON Address where
	parseJSON (Object v) = Address <$>
						  v .: "streetAddress" <*>
						  v .: "city" <*>
						  v .: "state" <*>
						  v .: "postalCode"
	-- A non-Object value is of the wrong type, so fail.
	parseJSON _ = mzero

instance FromJSON PhoneNumber where
	parseJSON (Object v) = PhoneNumber <$>
						  v .: "num_type" <*>
						  v .: "number"
	-- A non-Object value is of the wrong type, so fail.
	parseJSON _ = mzero

-- convert JSON to Haskell data 	
get_haskell :: JSONData -> Customers
get_haskell json_input = fromJust(decode json_input :: Maybe Customers)

-- HELPER FUNCTIONS
-- add maybe
maybe :: b -> (a -> b) -> Maybe a -> b
maybe n _ Nothing = n
maybe _ f (Just x) = f x

-- remove maybe and validate JSON structure
fromJust :: Maybe a -> a
fromJust Nothing = error "Invalid structure of a JSON file"
fromJust (Just x) = x

-- wrap tags around Haskell data
type TagName = String
type TagValue = String
type TaggedValue = XMLData
wrapper :: TagName->TagValue->TaggedValue
wrapper tagname tagValue =  "\n" ++ ['<'] ++ tagname ++ ['>']
							++ tagValue ++ 
							"</" ++ tagname ++ ['>']
-- form XML root element
type TagAttr = String
root_wrapper :: TagName->TagAttr->TagValue->TaggedValue
root_wrapper tagname tagAttr tagValue =  "\n" ++ ['<'] ++ tagname ++ " " ++ tagAttr ++ ['>']
										++ tagValue ++ 
										"</" ++ tagname ++ ['>']
							
-- get numbers from phoneNumbers array
xml_numbers :: [PhoneNumber] -> XMLData
xml_numbers [] = "\n"
xml_numbers (x:xs) = (wrapper "phoneNumber"
						( (wrapper "num_type" $ Text_M.unpack $ num_type $ x)
						  ++(wrapper "number" $ Text_M.unpack $number $x)
						  ++ "\n"
						) 
					  )++xml_numbers (xs)

-- get customers from customers array
xml_customers :: [Customer] -> XMLData
xml_customers [] = "\n"
xml_customers (x:xs) =  "\n"
						++(wrapper "customer"
						( (wrapper "firstName" $ Text_M.unpack $ firstName $ x)
						  ++(wrapper "lastName" $ Text_M.unpack $lastName $x)
						  ++ wrapper "age" (show $ age $x) 
						  
						  ++ wrapper "address" 
							(
							wrapper "streetAddress" (Text_M.unpack $ streetAddress $ address $ x) 
							++ wrapper "city" (Text_M.unpack $ city $ address $ x) 
							++ wrapper "state" (Text_M.unpack $ city $ address $ x) 
							++ wrapper "postalCode" (Text_M.unpack $ postalCode $ address $ x) ++ "\n"
							)
						    ++ wrapper "phoneNumbers" (xml_numbers $ phoneNumbers $ x) ++ "\n"
						)++ "\n"
					   )++xml_customers (xs)

-- add XML version for the XML output file
xml_version :: XMLData -> XMLData
xml_version = (++) "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"

-- READ/WRITE FILE
-- compile as ghc Main.hs -o executable_file_name
-- once execution file is compiled, run it as executable_file_name.exe inputFile outputFile
interactWith function inputFile outputFile = do
  json_input <- BSL_M.readFile inputFile		--can type in file.json
  writeFile outputFile (function json_input) 	--can type in file.xml, this will create a new file with specidied name if it doesn't exist

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [json_input,xml_output] -> interactWith function json_input xml_output
            _ -> putStrLn "error: exactly two arguments needed"

        -- main function transforming the input
        myFunction = get_xml
		
-- OUTPUT as XML
-- XML output retriever for sample data
xml_out :: XMLData
xml_out =  xml_version(root_wrapper "customers" "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"" (xml_customers $customers $ get_haskell json_in))

-- XML output to be written into file (read from file containing json)
get_xml :: JSONData -> XMLData
get_xml json_input = xml_version(root_wrapper "customers" "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"" (xml_customers $customers $ get_haskell json_input))