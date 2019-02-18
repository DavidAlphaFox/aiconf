-module(ai_conf).
-export([load_conf/2]).
-export([sections/1,section/2,section/3,value/3,value/4]).
load_conf(ConfName,Files)->
    ai_conf_server:load_conf(ConfName,Files).
sections(ConfName)->
    ai_conf_server:sections(ConfName).
section(ConfName,SectionKey)->
    ai_conf_server:section(ConfName,SectionKey,undefined).
section(ConfName,SectionKey,Default)->
    ai_conf_server:section(ConfName,SectionKey,Default).
value(ConfName,SectionKey,Key)->
    ai_conf_server:value(ConfName,SectionKey,Key,undefined).
value(ConfName,SectionKey,Key,Default)->
    ai_conf_server:value(ConfName,SectionKey,Key,Default).

   
