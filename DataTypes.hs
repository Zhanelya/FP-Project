-- ECS713, Group 2: Jean Emmanuel Yannick Messey-Elouma, Zhanelya Subebayeva, Soham Amrish Trivedi
module DataTypes where
-- DATA TYPES USED TO STORE AND PROCESS JSON DATA IN HASKELL FORMAT

-- Aeson imports
import qualified Data.Text as Text_M
import qualified Data.ByteString.Lazy as BSL_M

type JSONData = BSL_M.ByteString
type XMLData = String

-- AESON FromJSON
-- CONVERT JSON TO HASKELL DATA TYPE
data Customers = Customers
	{ customers	:: [Customer]
	} deriving Show
	
data Customer = Customer 
	{ firstName    :: Text_M.Text
		, lastName :: Text_M.Text
		, age      :: Int
		, address  :: Address
  		, phoneNumbers :: [PhoneNumber]
    } deriving Show
	
data Address = Address
	{ streetAddress :: Text_M.Text
		, city 		:: Text_M.Text
		, state 	:: Text_M.Text
		, postalCode:: Text_M.Text
	} deriving Show
	
data PhoneNumber = PhoneNumber
	{ num_type 	:: Text_M.Text
	   ,number	:: Text_M.Text
	} deriving Show


	
