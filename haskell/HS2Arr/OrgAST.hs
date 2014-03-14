module OrgAST where

type Number = Double
type Loc = Int

-- stop gap
type Hint = ()
type Variant = ()

data Program
  = S_program Loc [Header] Expr

data Header
  = S_import      {- l -} Loc {- file  -} ImportType {- name -} String
  | S_provide     {- l -} Loc {- block -} Expr
  | S_provide_all {- l -} Loc

data ImportType
  = S_file_import  {- file   -} String
  | S_const_import {- module -} String


data Expr
  = S_hint_exp   {- l -} Loc {- hints -} [Hint] {- e -} Expr
  | S_block      {- l -} Loc {- stmts -} [Expr]
  | S_user_block {- l -} Loc {- body -} Expr
  | S_fun
    {- l -} Loc
    {- name -} String
    {- params -} [String]
    {- args -} [Bind]
    {- ann -} Ann
    {- doc -} String
    {- body -} Expr
    {- check -} Expr

  | S_var     {- l -} Loc {- name -} Bind {- value -} Expr
  | S_let     {- l -} Loc {- name -} Bind {- value -} Expr
  | S_graph   {- l -} Loc {- bindings -} [Expr] -- list of S_let
  | S_when    {- l -} Loc {- test -} Expr {- block -} Expr
  | S_assign  {- l -} Loc {- id -} String {- value -} Expr
  | S_if      {- l -} Loc {- branches -} [IfBranch]
  | S_if_else {- l -} Loc {- branches -} [IfBranch] {- _else -} Expr
  | S_cases   {- l -} Loc {- type -} Ann {- val -} Expr {- branches -} [CasesBranch]
  | S_cases_else
    {- l -} Loc
    {- type -} Ann
    {- val -} Expr
    {- branches -} [CasesBranch]
    {- _else -} Expr

  | S_try        {- l -} Loc {- body -} Expr {- id -} Bind {- _except -} Expr
  | S_op         {- l -} Loc {- op -} String {- left -} Expr {- right -} Expr
  | S_check_test {- l -} Loc {- op -} String {- left -} Expr {- right -} Expr
  | S_not        {- l -} Loc {- expr -} Expr
  | S_paren      {- l -} Loc {- expr -} Expr
  | S_lam
    {- l -} Loc
    {- params -} [String]
    {- args -} [Bind]
    {- ann -} Ann
    {- doc -} String
    {- body -} Expr
    {- check -} Expr

  | S_method
    {- l -} Loc
    {- args -} [Bind]
    {- ann -} Ann
    {- doc -} String
    {- body -} Expr
    {- check -} Expr

  | S_extend        {- l -} Loc {- super -} Expr {- fields -} [Member]
  | S_update        {- l -} Loc {- super -} Expr {- fields -} [Member]
  | S_obj           {- l -} Loc {- fields -} [Member]
  | S_list          {- l -} Loc {- values -} [Expr]
  | S_app           {- l -} Loc {- _fun -} Expr {- args -} [Expr]
  | S_left_app      {- l -} Loc {- obj -} Expr {- _fun -} Expr {- args -} [Expr]
  | S_id            {- l -} Loc {- id -} String
  | S_num           {- l -} Loc {- n -} Number
  | S_bool          {- l -} Loc {- b -} Bool
  | S_str           {- l -} Loc {- s -} String
  | S_dot           {- l -} Loc {- obj -} Expr {- field -} String
  | S_get_bang      {- l -} Loc {- obj -} Expr {- field -} String
  | S_bracket       {- l -} Loc {- obj -} Expr {- field -} Expr
  | S_colon         {- l -} Loc {- obj -} Expr {- field -} String
  | S_colon_bracket {- l -} Loc {- obj -} Expr {- field -} Expr
  | S_data
    {- l -} Loc
    {- name -} String
    {- params -} [String]
    {- mixins -} [Expr]
    {- variants -} [Variant]
    {- shared_members -} [Member]
    {- check -} Expr

  | S_datatype
    {- l -} Loc
    {- name -} String
    {- params -} [String]
    {- variants -} [Variant]
    {- check -} Expr

  | S_for
    {- l -} Loc
    {- iterator -} Expr
    {- bindings -} [ForBind]
    {- ann -} Ann
    {- body -} Expr

  | S_check {- l -} Loc {- body -} Expr

data Bind
  = S_bind {- l -} Loc {- id -} String {- ann -} Ann

data Member
  = S_data_field    {- l -} Loc {- name -} Expr {- value -} Expr
  | S_mutable_field {- l -} Loc {- name -} Expr {- ann -} Ann {- value -} Expr
  | S_once_field    {- l -} Loc {- name -} Expr {- ann -} Ann {- value -} Expr
  | S_method_field
    {- l -} Loc
    {- name -} Expr
    {- args -} [Bind]
    {- ann -} Ann
    {- doc -} String
    {- body -} Expr
    {- check -} Expr

data ForBind  = S_for_bind  {- l -} Loc {- bind -} Bind {- value -} Expr
data IfBranch = S_if_branch {- l -} Loc {- test -} Expr {- body -} Expr

data CasesBranch
  = S_cases_branch {- l -} Loc {- name -} String {- args -} [Bind] {- body -} Expr

data Ann
  = A_blank
  | A_any
  | A_name   {- l -} Loc {- id -} String
  | A_arrow  {- l -} Loc {- args -} [Ann] {- ret -} Ann
  | A_method {- l -} Loc {- args -} [Ann] {- ret -} Ann
  | A_record {- l -} Loc {- fields -} [AField]
  | A_app    {- l -} Loc {- ann -} Ann {- args -} [Ann]
  | A_pred   {- l -} Loc {- ann -} Ann {- exp -} Expr
  | A_dot    {- l -} Loc {- obj -} String {- field -} String

data AField = A_field {- l -} Loc {- name -} String {- ann -} Ann
