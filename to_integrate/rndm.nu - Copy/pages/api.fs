namespace rndmnu.pages
module API =
    open Strings.html
    let page =
        H2 ((Uni "API"), [
                URLParam (Uni "rndm.nu/{*language*/}{*mode*}[*mode options*]{.*format*}")
                P (Trans { 
                    en = "rndm.nu provides a simple URL based API to retrieve true random numbers in all sorts of formats. The API is easy to use for both humans and programs alike. Above is the basic format which every API request to rndm.nu must use. Parameter names are are in *bold*. Optional parameters are surrounded by {curly brackets}. Mandatory parameters are surrounded by [square brackets]. Below the parameters and how to use them are described in detail with examples for each use case."
                    ja = ""
                })
                H4 ((Uni "Mode"), [
                        P (Trans { 
                            en = "[*mode*] where mode must be either '*standard*', '*binary*', '*shuffle*' or '*lottory*'"
                            ja = ""
                        })
                        P (Trans { 
                            en =  "If mode is not provided it will default to 'standard'. Options for each mode are described below:"
                            ja = ""
                        })
                ])
                H3 ((Uni "Modes"), [
                        H4 ((Uni "Standard Mode"), [
                                URLParam (Uni "/[range]{xmultiplier}{.format}")
                                P (Trans { 
                                    en =  "Standard mode returns well formatted random numbers with many formatting options."
                                    ja = ""
                                })
                                CodeEg [
                                    "a"
                                    "b"
                                    "c"
                                ]
                        ])
                ])
        ])
    