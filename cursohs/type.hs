type Host = String
type Username = String
type Repository = String
type Git = (Username,Host,Repository)

git :: Git
git = ("pacoca51","github.com","hs")

account (u,h,r) = u++"@"++h
https (u,h,r) = h++"/"++u++"/"++r
repo (u,h,r) = u++"/"++r