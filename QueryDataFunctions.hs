-- ECS713, Group 2: Jean Emmanuel Yannick Messey-Elouma, Zhanelya Subebayeva, Soham Amrish Trivedi
module QueryDataFunctions where
-- FUNCTIONS TO QUERY PARSED DATA

import qualified Data.Text as Text_M
import DataTypes

-- get age of a customer by customer's first name and last name from parsed data (haskell data)
-- run get_age fname lname $ get_haskell json_in
get_age :: String -> String -> Customers -> String
get_age fname lname haskell_data =  customer_age $ customers $ haskell_data 
    where 
	customer_age xs = case xs of [] -> "Customer not found"
	                             (x:xs) -> if (get_Text_field firstName x == fname && get_Text_field lastName x == lname) 
											then show(age x) 
											else customer_age (xs)
		
-- get average customer age from parsed data (haskell data)
-- run avg_customer_age $ get_haskell json_in	
avg_customer_age :: Customers -> Float
avg_customer_age haskell_data = x_avg_customer_age 0 0 (customers haskell_data) 
	where 
	x_avg_customer_age 0 0 [] = error "No customers found"
	x_avg_customer_age num sum [] = fromIntegral (sum) / num 
	x_avg_customer_age num sum (x:xs) = x_avg_customer_age (num + 1) (sum + (age x)) xs

-- get minimum customer age from parsed data (haskell data)
-- run min_customer_age $ get_haskell json_in	
min_customer_age :: Customers -> Int
min_customer_age haskell_data = minimum $ get_ages $ customers $ haskell_data
	where 
	get_ages [] = []
	get_ages (x:xs) = (age x) : get_ages (xs)
	
-- (helper function) get a concrete text field of a given customer, for instance:
-- get_Text_Field firstName customer
type TextField = Customer->Text_M.Text
get_Text_field :: TextField -> Customer -> String
get_Text_field field customer = Text_M.unpack $ field $ customer 

-- each customer's firstName, lastName and street address zip
-- run get_addresses $ get_haskell json_in	
type FName = Text_M.Text
type LName = Text_M.Text
type CustomerAddress = Text_M.Text
get_addresses :: Customers -> [(FName, LName, CustomerAddress)]
get_addresses haskell_data = zip3 (map firstName ( customers $ haskell_data ))
								  (map lastName ( customers $ haskell_data ))
								  (map streetAddress ((map address) ( customers $ haskell_data )))
								  
-- count customers living in a certain postalCode
-- run cnt_cust_in_postcode "10021" $ get_haskell json_in
-- which would result in 2 customers
type Postcode = String
cnt_cust_in_postcode :: Postcode -> Customers -> Int
cnt_cust_in_postcode postcode haskell_data = length $ filter (==Text_M.pack(postcode)) (map postalCode ((map address) ( customers $ haskell_data )))