import Data.List
import Data.Maybe
import Data.Text (splitOn, pack, unpack)


data File = File {
    fileName :: String,
    fileContent :: String
} deriving (Show, Eq)

data Folder = Folder {
    folderName :: String,
    subFolders :: [Folder],
    subFiles :: [File]
} deriving (Show, Eq)

data FileSystem = FileSystem {
    current :: String,
    root :: Folder
} deriving (Show, Eq)

data CommandResult = CommandResult {
    fileSystem :: FileSystem,
    output :: String
} deriving (Show, Eq)

data Command = Pwd 
    | Ls  { dir :: String } -- dir is "" or relative/path/ or /full/path/
    | Cd  { path :: String } -- path is relative/path/ or /full/path/ or has ..
    | Cat { contentToCat :: [String] } -- contentToCat = [paths or >] 
    | Rm  { files :: [String] }
    | Write { file :: String, content :: String }
    | Invalid deriving (Show, Eq)

--- Parse

readCommand :: [String] -> Command
readCommand ("pwd":[])    = Pwd
readCommand ("pwd":args)  = Invalid
readCommand ("cd":[])     = Invalid
readCommand ("cd":arg:args)  = Cd arg
readCommand ("ls":[])     = Ls ""
readCommand ("ls":arg:args)  = Ls arg
readCommand ("rm":[])     = Invalid
readCommand ("rm":args)   = Rm args
readCommand ("cat":[])    = Invalid
readCommand ("cat":args)  = Cat args
readCommand ("wr":[])     = Invalid
readCommand ("wr":a:[])   = Invalid
readCommand ("wr":a:b)    = Write a (intercalate " " b)
readCommand _             = Invalid


--- Path calculation

--gives a proper splitterd path starting from root
resolveHelper :: [String] -> [String]
resolveHelper [] = []
resolveHelper (a:[]) = [a]
resolveHelper (a:"..":c) = resolveHelper c
resolveHelper (a:".":c)  = resolveHelper $ [a] ++ c
resolveHelper (a:b:c)    = [a] ++ (resolveHelper $ [b] ++ c)


resolvePath :: String -> [String]
resolvePath path = filter (\h -> h /= ".." && h /= "." && h /= "") $ resolveHelper $ [""] ++ (filter (\x -> length x > 0) splitted)
    where   splitted = map (\x -> unpack x) $ splitOn delimiter textPath
            delimiter = pack "/"
            textPath = pack path



calcPath :: String -> String -> [String]
calcPath curr path@('/':rest) = resolvePath path       --for paths starting with /
calcPath curr path = resolvePath (curr ++ "/" ++ path)

--- FileSystem getters


getSubfolder :: Folder -> String -> Maybe Folder
getSubfolder fldr subdir 
        |res == []  = Nothing
        |otherwise  = Just (head res)
        where res = filter (\x -> folderName(x) == subdir) $ subFolders(fldr)

--returns a folder by a given list of arguments starting from the root
getFolder :: FileSystem -> [String] -> Maybe Folder
getFolder fs [] = Just $ root fs
getFolder fs (x:xs) = if (elem x (map (\x-> (folderName x)) $ subFolders $ root fs)) 
                    then getFolder (FileSystem "" (fromJust (getSubfolder (root fs) x))) xs
                    else Nothing

--returns a new file system with @fullPath as directory in order to change directory of an exisitng one whose root is @root
getNewFileSystem :: Folder -> Folder -> [String] -> String -> Maybe FileSystem
getNewFileSystem root currentFolder [] fullPath = Just (FileSystem fullPath root)
getNewFileSystem root currentFolder (x:xs) fullPath = 
    let nextFolder = getSubfolder currentFolder x in
         if (nextFolder == Nothing) then Nothing else getNewFileSystem root (fromJust nextFolder) xs 
         (fullPath ++ "/" ++ folderName(fromJust nextFolder))


--- Functions

-- This function is used for readability
pwd = current 

--  rm

copyOfFoldersRm :: [Folder] -> [String] -> [Folder] 
copyOfFoldersRm [] _ = []
copyOfFoldersRm (x:xs) path = copyOfFolderRm x path : copyOfFoldersRm xs path

copyOfFolderRm :: Folder -> [String] -> Folder
copyOfFolderRm x [] = Folder (folderName x) (copyOfFoldersRm (subFolders x) []) (subFiles x) 
copyOfFolderRm x (p:path) = if (p == folderName x) 
                        then if (length path == 1 && (elem (head path) (map (\y -> fileName y) (subFiles x))))
                             then Folder (folderName x) (copyOfFoldersRm (subFolders x) []) (removeGivenFile (head path) (subFiles x))
                             else Folder (folderName x) (copyOfFoldersRm (subFolders x) path) (subFiles x) 
                        else Folder (folderName x) (copyOfFoldersRm (subFolders x) []) (subFiles x)
                        where removeGivenFile name files = (filter (\y -> name /= fileName y) files)

rm :: FileSystem -> [String] -> CommandResult
rm fs path = CommandResult (FileSystem (current fs) (copyOfFolderRm (root fs) path)) ""



--  cd


cdHelper :: FileSystem -> [String] -> CommandResult
cdHelper fs [] = CommandResult fs ""
cdHelper fs l = let originalRoot = root(fs) 
                in if newStuff == Nothing then CommandResult fs "Incorrect path" else CommandResult (fromJust newStuff) ""
                    where newStuff = (getNewFileSystem (root fs) (root fs) l "")

cd :: FileSystem -> [String] -> CommandResult
cd fs [] = CommandResult (FileSystem "/" (root fs)) ""
cd fs l = cdHelper fs l


--  ls


ls :: FileSystem -> [String] -> CommandResult
ls fs [] = CommandResult fs (lsHelper $ root fs)
ls fs path = if ((getFolder fs path) == Nothing) then CommandResult fs "Inexisting path"
            else CommandResult fs (lsHelper $ fromJust (getFolder fs path))
    
lsHelper :: Folder -> String
lsHelper folder = intercalate " " ((map (\x-> (folderName x)) $ subFolders folder) ++ (map (\y-> (fileName y)) $ subFiles folder))

-- write

-- copy functions used in writing into file
copyOfFoldersWrite :: [Folder] -> [String] -> String -> Bool -> [Folder] 
copyOfFoldersWrite [] _ _ _= []
copyOfFoldersWrite (x:xs) path content append= (copyOfFolderWrite x path content append) : (copyOfFoldersWrite xs path content append)

copyOfFolderWrite :: Folder -> [String] -> String -> Bool -> Folder
copyOfFolderWrite current [] content append = Folder (folderName current) (copyOfFoldersWrite (subFolders current) [] content append) (subFiles current) 
copyOfFolderWrite current (p:restOFPath) content append =    if (p == folderName current) 
                                                then    if (length restOFPath == 1 && (not (elem (head restOFPath) (map (\y -> fileName y) (subFiles current) )))) -- we are in the dir and file does not exist
                                                        then Folder (folderName current) (copyOfFoldersWrite (subFolders current) [] content append) ((subFiles current) ++ [File (head restOFPath) content] )
                                                        else Folder (folderName current) (copyOfFoldersWrite (subFolders current) restOFPath content append)
                                                         (map (\f -> if (fileName f) == firstOfRest then File (fileName f) (replacement f) else f) (subFiles current))  
                                                else Folder (folderName current) (copyOfFoldersWrite (subFolders current) [] content append) (subFiles current)
                                                                where firstOfRest = head restOFPath
                                                                      replacement f = if append then ((fileContent f) ++ content) else content


write :: FileSystem -> [String] -> String -> Bool -> CommandResult
write fs path content append = CommandResult (FileSystem (current fs) newRoot) ""
                        where newRoot = copyOfFolderWrite (root fs) ("/" : path) content append


--  cat


cat :: FileSystem -> [String] -> CommandResult
cat fs [] = CommandResult fs "Illegal arguments to function cat"
cat fs path = if ((getFolder fs allButLast) == Nothing) 
            then CommandResult fs "File does not exist" 
            else if ((getFileCont (fromJust (getFolder fs allButLast)) $ last path) == Nothing)
                then CommandResult fs "File does not exist"
                else CommandResult fs (fromJust (getFileCont (fromJust (getFolder fs allButLast)) $ last path))
                where   allButLastLength = (length path) - 1
                        allButLast = take allButLastLength path

getFileCont :: Folder -> String -> Maybe String
getFileCont folder name = if (elem name (map (\x -> (fileName x)) $ subFiles folder))
                        then Just $ fileContent $ head (filter (\x -> (fileName x) == name) $ subFiles folder)
                        else Nothing


--- Execute


executeCat :: [String] -> FileSystem -> String -> CommandResult
executeCat []           fs res = CommandResult fs res
executeCat (">":save:rest) fs res = write fs (calcPath (pwd fs) save) res True
executeCat (file:rest)  fs res = executeCat rest fs (res ++ output curr) -- stack contents in result
                                    where curr = cat fs $ calcPath (pwd fs) file

executeRm :: [String] -> FileSystem -> String -> CommandResult
executeRm []       fs res = CommandResult fs res
executeRm (f:args) fs res = executeRm args newFs newRes 
                                where exec   = rm fs (["/"] ++ (calcPath (pwd fs) f)) 
                                      newFs  = fileSystem exec
                                      newRes = res ++ (output exec)

execute :: Command -> FileSystem -> CommandResult
execute (Invalid)   fs = CommandResult fs "Invalid command."
execute (Pwd)       fs = CommandResult fs (pwd fs)
execute (Ls  path)  fs = ls fs $ calcPath (pwd fs) path
execute (Cd  path)  fs = cd fs $ calcPath (pwd fs) path
execute (Write f c) fs = write fs (calcPath (pwd fs) f) c False
execute (Rm  args)  fs = executeRm  args fs ""
execute (Cat args)  fs = executeCat args fs ""

--- Main


start :: FileSystem -> IO FileSystem
start fs = do 
    line <- getLine
    let args = readCommand (words line)
        res = execute args fs
    print (output res)
    start (fileSystem res)

--- Sample Filesystem

fs = FileSystem "/" (Folder "/" [
                        Folder "test" [
                            (Folder "firmata" 
                                [(Folder "kvo" [] [])] 
                                [File "ne" "Content na file 'ne'"]),
                            (Folder "e" [] [(File "mnogo" "Content na file 'mnogo'"), (File "eha" "Ehaa tova stana gadno")]),
                            (Folder "mnogo" 
                                [(Folder "seriozna" 
                                    [(Folder "top" [] [])] [])]
                                [])  ]
                             []
    ] [])

