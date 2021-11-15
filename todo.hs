module Todo where

data Task = Task {
    description :: String,
    date :: String,
    completed :: Bool
} deriving (Show)

type Description = String
type Date = String
type Completed = Bool
type Todo = [Task]

completeTask :: Description -> Todo -> Todo
completeTask _ [] = []
completeTask description (Task desc date done:todos)
    | desc == description = (Task desc date True) : todos
    | otherwise = (Task desc date done) : (completeTask description todos)

incompleteTask :: Description -> Todo -> Todo
incompleteTask _ [] = []
incompleteTask description (Task desc date done:todos)
    | desc == description = (Task desc date False) : todos
    | otherwise = (Task desc date done) : (completeTask description todos)

insertTask :: Task -> Todo -> Todo
insertTask task todo = task : todo

createTask :: Description -> Date -> Task
createTask description date = Task description date False

modifyTask :: Description -> Task -> Todo -> Todo
modifyTask _ _ [] = []
modifyTask desc newTask (task : tasks)
    | desc == description task = newTask : tasks
    | otherwise = task : (modifyTask desc newTask tasks)

filterTaskByDescription::Description -> Todo ->Todo
filterTaskByDescription _ [] = []
filterTaskByDescription desc (Task description date completed:todos)
    | desc == description = (Task description date completed) : (filterTaskByDescription desc todos)
    | otherwise = (filterTaskByDescription desc todos)

filterCompletedTasks::Todo ->  Todo
filterCompletedTasks [] = []
filterCompletedTasks (Task description date completed : todos)
  | completed == True = (Task description date completed) : (filterCompletedTasks todos)
  | otherwise = (filterCompletedTasks todos)

main = do
    loop "help" []

loop :: String -> Todo -> IO ()
loop "help" todo = do
    putStrLn helpMsg
    command <- readCommand
    loop command todo
loop "exit" todo = do
    putStrLn "Goodbye!! Remember your TODO's"
    return ()
loop "insert" todo = do
    task <- readTask
    command <- readCommand
    loop command $ insertTask task todo
loop "complete" todo = do
    description <- readDescription
    command <- readCommand
    loop command $ completeTask description todo
loop "incomplete" todo = do
    description <- readDescription
    command <- readCommand
    loop command $ incompleteTask description todo
loop "modify" todo = do
    description <- readDescription
    task <- readTask
    command <- readCommand
    loop command $ modifyTask description task todo
loop "debug" todo = do -- TODO: Remove this command
    putStrLn $ show todo
    command <- readCommand
    loop command todo
loop "show completed tasks" todo = do
    putStrLn $ show todo
    command <- readCommand
    loop command $ filterCompletedTasks todo
loop "show tasks by description" todo = do
    description <- readDescription
    command <- readCommand
    putStrLn $ show todo
    loop command $ filterTaskByDescription description todo  
loop _ todo = do
    putStrLn "Incorrect command"
    command <- readCommand
    loop command todo

readDescription :: IO String
readDescription = do
    putStr "Description (id) <- "
    getLine

readDate :: IO String
readDate = do
    putStr "Date <- "
    getLine

readTask :: IO Task
readTask = do
    description <- readDescription
    date <- readDate
    return $ createTask description date

readCommand :: IO String
readCommand = do
    putStr "Command -> "
    getLine

helpMsg :: String
helpMsg = unlines [
    "help",
    "\t - Show existing commands and descriptions",
    "insert",
    "\t - Creates and inserts a Task",
    "complete",
    "\t - Marks task as done",
    "incomplete",
    "\t - Marks task as incomplete",
    "modify",
    "\t - modifies contents of task",
     "Show completed tasks",
    "\t - shows completed tasks",
    "show taks by description",
    "\t - Shows tasks by description"
    ]
