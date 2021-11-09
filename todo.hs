module Todo (
    -- Task,
    -- Todo,
    -- completeTask,
    -- createTask,
    -- replaceTask,
    -- removeTask,
    -- showTasks,
    -- showTasksByDate,
    -- showTask,
    -- showCompletedTasks,
    -- showDueTasks
) where

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

createTask :: Description -> Date -> Todo -> Todo
createTask description date todo = (Task description date False) : todo

modifyTask :: Description -> Task -> Todo -> Todo
modifyTask _ _ [] = []
modifyTask desc newTask (task : tasks)
    | desc == description task = newTask : tasks
    | otherwise = task : (modifyTask desc newTask tasks)
