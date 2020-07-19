module Assignment2 (transaction_to_string, trade_report_list, stock_test, get_trades, trade_report, update_money, profit, profit_report, complex_profit_report) where


type Transaction = (Char, Int, Int, String, Int) 

test_log :: [Transaction]
test_log = [('B', 100, 1104,  "VTI",  1),
            ('B', 200,   36, "ONEQ",  3),
            ('B',  50, 1223,  "VTI",  5),
            ('S', 150, 1240,  "VTI",  9),
            ('B', 100,  229, "IWRD", 10),
            ('S', 200,   32, "ONEQ", 11), 
            ('S', 100,  210, "IWRD", 12)
            ]


-- Part A

--this function converts transactions given into legible and understandable text for any reader
--this should consist of a simple if statement
--if b then text for bought, else text for sold
transaction_to_string :: Transaction -> String
transaction_to_string (action, units, price, stock, day) = if action == 'B' 
                                                            then "Bought " ++ show units ++ " units of " ++ stock ++ " for " ++ show price ++ " pounds each on day " ++ show day
                                                            else "Sold " ++ show units ++ " units of " ++ stock ++ " for " ++ show price ++ " pounds each on day " ++ show day

--this function needs to be able to display a list of transactions from test_log and conver each one into a string
--this has to be done using a map function, which is relatively simple anyway
trade_report_list :: [Transaction] -> [String]
trade_report_list list = map transaction_to_string list

--this function needs to return a bool depending on whether the stock of a specific type is traded in the given transaction
--we will again do this using a simple if statement
stock_test :: String -> Transaction -> Bool
stock_test stocks (action, units, price,stock, day) = if stocks == stock
                    then True
                    else False

--this function will need to take a stock transaction log and return all of the trades for a given stock
--this should obviously be done using the previous stock_test function and the filter function
--again this is relatively simple using the syntax learnt for filter
get_trades :: String -> [Transaction] -> [Transaction]
get_trades stock transaction_log = ((filter(\ x -> stock_test(stock) (x))) transaction_log)

--this function needs to take a stock and a transaction_log, returning a string containing the history of a specific stock
--this needs to be done using map and unlines
--we are going to apply the transaction_to_string function to our trade history, which we will retrieve using the get_trades function
trade_report :: String -> [Transaction] -> String
trade_report stock transaction_log = unlines (map (transaction_to_string) (get_trades(stock) transaction_log))



-- Part B

--this function will update our money given a transaction and our starting money
--this will be done using a similar if statement to question 1
--remembering to subtract money if stock is bought not add it
update_money :: Transaction -> Int -> Int
update_money (action, units, price, stock, day) money = if action == 'B'
                                                            then money - (units * price)
                                                            else money + (units * price)

--this function needs to find the profit/loss using a transaction log and the name of a stock
--using the foldr function and our transaction
--if the stock name matches the required stock, apply the update_money function to our transaction
profit :: [Transaction] -> String -> Int
profit transaction_log stock_name = foldr(\(action, units, price, stock, day) acc -> if stock == stock_name then update_money(action, units, price, stock, day) acc else acc) 0 transaction_log

--this function needs tp return a profit/loss report readable by anyone, using a list of stocks and a transaction_log using the map and unlines functions
--we are going to use an anonymous function in order to achieve a similar output as found in question 5
--we are going to apply the profit function to transaction_log and stock 
profit_report :: [String] -> [Transaction] -> String
profit_report stock_list transaction_log = unlines (map (\stock -> stock ++ ": " ++ show(profit(transaction_log) stock))stock_list)





-- Part C


test_str_log = "BUY 100 VTI 1\nBUY 200 ONEQ 3\nBUY 50 VTI 5\nSELL 150 VTI 9\nBUY 100 IWRD 10\nSELL 200 ONEQ 11\nSELL 100 IWRD 12\n"



type Prices = [(String, [Int])]

test_prices :: Prices
test_prices = [
                ("VTI", [1689, 1785, 1772, 1765, 1739, 1725, 1615, 1683, 1655, 1725, 1703, 1726, 1725, 1742, 1707, 1688, 1697, 1688, 1675]),
                ("ONEQ", [201, 203, 199, 199, 193, 189, 189, 183, 185, 190, 186, 182, 186, 182, 182, 186, 183, 179, 178]),
                ("IWRD", [207, 211, 213, 221, 221, 222, 221, 218, 226, 234, 229, 229, 228, 222, 218, 223, 222, 218, 214])
              ]



complex_profit_report :: String -> Prices -> String
--pseudo code to break down task
--take test_str_log and test_prices
--use 3rd string from list of strings(test_str_log) to find stock name (stock)
--use 4th string from list of strings(test_str_log) to find day stock was bought or sold (day)
--use first character of first string from list of strings(test_str_log) to find action
--use new stock to find stock_prices from test_prices and new day to find price on required day 
--use 2nd string from list of strings(test_str_log) to find quantity of stock (units)
--input these into profit_report recursively every 4 strings from list of strings(test_str_log)
complex_profit_report = error "Not implemented"
        

