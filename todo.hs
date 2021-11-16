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
    | otherwise = (Task desc date done) : (incompleteTask description todos)

insertTask :: Task -> Todo -> Todo
insertTask task todo = task : todo

createTask :: Description -> Date -> Task
createTask description date = Task description date False

modifyTask :: Description -> Task -> Todo -> Todo
modifyTask _ _ [] = []
modifyTask desc newTask (task : tasks)
    | desc == description task = newTask : tasks
    | otherwise = task : (modifyTask desc newTask tasks)

removeTask :: Description -> Todo -> Todo
removeTask _ [] = []
removeTask desc (Task description date done: todos)
    | desc == description =(removeTask desc todos)
    | otherwise = (Task description date done) : (removeTask desc todos)

removeTasksByDate :: Date -> Todo -> Todo
removeTasksByDate _ [] = []
removeTasksByDate date (Task description dte done: todos)
    | date == dte = (removeTasksByDate date todos)
    | otherwise = (Task description dte done) : (removeTasksByDate date todos)

filterTaskByDescription :: Description -> Todo ->Todo
filterTaskByDescription _ [] = []
filterTaskByDescription desc (Task description date completed:todos)
    | desc == description = (Task description date completed) : (filterTaskByDescription desc todos)
    | otherwise = (filterTaskByDescription desc todos)

filterCompletedTasks :: Todo ->  Todo
filterCompletedTasks [] = []
filterCompletedTasks (Task description date completed : todos)
  | completed == True = (Task description date completed) : (filterCompletedTasks todos)
  | otherwise = (filterCompletedTasks todos)

filterIncompleteTasks :: Todo ->  Todo
filterIncompleteTasks [] = []
filterIncompleteTasks (Task description date completed : todos)
  | completed == False = (Task description date completed) : (filterIncompleteTasks todos)
  | otherwise = (filterIncompleteTasks todos)

filterTasksByDate :: Date -> Todo -> Todo
filterTasksByDate _ [] = []
filterTasksByDate date (Task description dte done: todos)
    | date == dte = (Task description dte done) : (filterTasksByDate date todos)
    | otherwise = (filterTasksByDate date todos)

showTasks :: Todo -> IO ()
showTasks [] =do
    return ()
showTasks (task:todo)= do
    putStrLn $ show task
    showTasks todo

main = do
    loop "pass" []

loop :: String -> Todo -> IO ()
loop "pass" todo = do
    command <- readCommand
    loop command todo
loop "help" todo = do
    putStr helpMsg
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
loop "remove" todo = do
    description <- readDescription
    command <- readCommand
    loop command $ removeTask description todo
loop "remove date" todo = do
    date <- readDate
    command <- readCommand
    loop command $ removeTasksByDate date todo
loop "show" todo = do
    showTasks todo
    command <- readCommand
    loop command todo
loop "show completed" todo = do
    showTasks $ filterCompletedTasks todo
    command <- readCommand
    loop command todo
loop "show incomplete" todo = do
    showTasks $ filterIncompleteTasks todo
    command <- readCommand
    loop command todo
loop "show description" todo = do
    description <- readDescription
    showTasks $ filterTaskByDescription description todo
    command <- readCommand
    loop command todo
loop "show date" todo = do
    date <- readDate
    showTasks $ filterTasksByDate date todo
    command <- readCommand
    loop command todo
loop _ todo = do
    putStrLn "Incorrect command - write help"
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
    "exit",
    "\t - Exit the program",
    "insert",
    "\t - Creates and inserts a Task",
    "complete",
    "\t - Marks task as done",
    "incomplete",
    "\t - Marks task as incomplete",
    "modify",
    "\t - Modifies contents of task",
    "remove",
    "\t - Removes task with matching description",
    "remove date",
    "\t - Removes task with matching date",
    "show",
    "\t - Show all the tasks",
    "show completed",
    "\t - Shows completed tasks",
    "show incomplete",
    "\t - Shows incomplete tasks",
    "show description",
    "\t - Shows tasks by description",
    "show date",
    "\t - Shows tasks with matching date"
    ]
