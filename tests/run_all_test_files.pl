:- prolog_load_context(directory, CurDir),  % current directory
    file_directory_name(CurDir, BaseDir),   % project base directory
    asserta( base_path(BaseDir) ).          % add clause to the front of database

:- use_module(library(filesex)).    % directory_file_path/3

:- initialization(main).

main :-
    load_test_files.

load_test_files :-
    base_path(Base),
    directory_file_path(Base, "tests", TestPath),
    % get all the files in the test directory
    directory_files(TestPath, Files),
    load_files(Files).

%-----------------------------------------------------------------------%

% all test files done
load_files([]).

load_files([File|T]) :-
    % make sure it is a test file
    atom_concat(_, '.plt', File),
    % get the path
    base_path(Base),
    directory_file_path(Base, "tests/", TestPath),
    directory_file_path(TestPath, File, TestFilePath),
    % load it
    consult(TestFilePath),
    % look for others
    load_files(T).

% was not a test file
load_files([_|T]) :-
    load_files(T).
