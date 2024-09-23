{-# LANGUAGE DeriveGeneric #-}
module Utils (leerUsuarios, validarUsuario, Usuario(..)) where

import Data.Aeson (eitherDecode, FromJSON)
import GHC.Generics (Generic)
import Data.List (find)
import qualified Data.ByteString.Lazy as B

data Usuario = Usuario { usuarioId :: String, nombreCompleto :: String, puesto :: String } deriving (Show, Generic)

instance FromJSON Usuario

leerUsuarios :: FilePath -> IO (Either String [Usuario])
leerUsuarios filePath = do
    contenido <- B.readFile filePath
    return $ eitherDecode contenido

validarUsuario :: String -> [Usuario] -> Maybe Usuario
validarUsuario idUsuario usuarios = find (\u -> usuarioId u == idUsuario) usuarios
