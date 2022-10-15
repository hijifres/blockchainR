#' R6 Class representing a blockchain
#' @importFrom R6 R6Class
#' @import dplyr
#' @import openssl
#' @export
blockchain <- R6Class("blockchain",

                      public = list(
                        #' @field blocks List of blocks.
                        blocks = list(),
                        #' @description
                        #' Create a new blockchain object.
                        #' @param ... Extra parameters
                        #' @return A new `blockchain` object.
                        initialize = function(...){
                          #Create first block
                          self$add_block()
                          # Info about blockchain
                          self$greet()

                        },
                        #' @description
                        #' Initialization messag
                        #' @return A initialization message.
                        greet = function() cat(sprintf("Block chain inizialized, genesis block created \nBlockchain length %s",private$lengthblock())),

                        #' @description
                        #' Hash function sha256
                        #' @param value Object to hash
                        #' @importFrom digest digest
                        #' @return A hashed char
                        hash = function(value) digest(c(value),"sha256"),
                        #' Validation function searching a nonce for a hash beggining with 000
                        #' @description
                        #' Validate
                        #' @param value Object to validate
                        #' @return Object validate and hashed
                        validation = function(value){

                          while ( substr(self$hash(value),1,3 ) != "000" ) {
                            value$nonce = value$nonce +1
                          }
                          value$hash = self$hash(value)
                          return(value)
                        },
                        #' Genesis block generation
                        #' @description
                        #' Generate first block
                        #' @return NULL
                        genenis_block = function() {
                          block_genesis = list(index = 1,
                                               nonce = 0,
                                               timestamp = Sys.time(),
                                               data = "Genesis Block",
                                               previous_hash = "0")
                          block_genesis = self$validation(block_genesis)
                        },
                        #' Generate a block
                        #' @description
                        #' Generate a block
                        #' @return NULL
                        new_block = function(){

                          new_block <- list(index = self$blocks[[base::length(self$blocks)]]$index+1,
                                            nonce = 0,
                                            timestamp = Sys.time(),
                                            data = self$transaction_queue ,
                                            previous_hash = self$blocks[[base::length(self$blocks)]]$hash)

                          private$newBlock <- self$validation(new_block)
                          self$print_block(private$newBlock)
                          invisible(self)

                        },
                        #' Function to check the chain validation
                        #' @description
                        #' Generate a block
                        #' @return NULL
                        chain_validation = function(){

                          test= lapply(1:length(self$blocks), function(x){
                            if(self$hash(self$blocks[[x]][which(names(self$blocks[[base::length(self$blocks)]]) != "hash")]) != self$blocks[[x]]$hash){
                              return(FALSE)
                            }
                            if(x>1) {
                              if(self$blocks[[x]]$previous_hash != self$blocks[[x-1]]$hash)
                                return(FALSE)
                            }
                            return(TRUE)
                          })

                          if(FALSE %in%  unlist(test) ){
                            message("Falsified blocks")
                            return(which(unlist(test)==FALSE))
                          }

                          cat("Valid blockchain")
                          return(TRUE)
                        },
                        #' Add the block to chain
                        #' @description
                        #' Add a block
                        #' @return NULL
                        add_block = function() {


                          #Validation
                          if(base::length(self$blocks) == 0){
                            value = self$genenis_block()
                          } else{
                            if(base::length(private$newBlock) ==0 )
                              stop("Empty block list create a block first")
                            value = private$newBlock
                          }



                          if(base::length(self$blocks) >0){

                            if(value$previous_hash != self$hash(self$blocks[[base::length(self$blocks)]][which(names(self$blocks[[base::length(self$blocks)]]) != "hash")]) )
                              stop("invalid block-wrong hash")
                            if(substr(value$hash,1,3) != "000"  )
                              stop("invalid block-not signed")
                          }

                          # Add new block
                          self$blocks[[ base::length(self$blocks) +1 ]] = value

                          private$newBlock <- list()
                          self$transaction_queue <- list()

                          self$chain_info()

                          invisible(self)
                        },

                        #' Extract public key
                        #' @description
                        #' Extract public key
                        #' @param path key
                        #' @return Encoded base64 puk key
                        get_address = function(path){
                          base64_encode(read_key(path)$pubkey)
                        },


                        #' Check bala
                        #' @description
                        #' Extract public key
                        #' @param address key
                        #' @return Encoded base64 puk key
                        check_balance = function(address) {

                          balances = self$balances()
                          balance = balances[balances$id==self$get_address(path_private),]$balance

                        },

                        #' Check transaction
                        #' @description
                        #' Check transaction
                        #' @param transaction transaction
                        #' @return Encoded base64 puk key
                        check_transaction = function(transaction){
                          signature = transaction$signature
                          transaction$signature <- NULL
                          check = signature_verify(charToRaw(jsonlite::toJSON(transaction)), base64_decode(signature), pubkey = base64_decode(transaction$sender))
                          return(check)
                        },

                        #' New transaction
                        #' @description
                        #' New transaction
                        #' @param path_private private key
                        #' @param receiver pub key recever
                        #' @param amount amount
                        #' @return NULL
                        new_transaction = function(path_private, receiver, amount){
                          transaction = list(
                            sender = self$get_address(path_private),
                            receiver = receiver,
                            amount = amount
                          )
                          signature = signature_create(charToRaw(jsonlite::toJSON(transaction)), key = read_key(path_private) )

                          transaction$signature = base64_encode(signature)

                          if(self$check_transaction(transaction)!=T)
                            stop("Not signed")

                          self$transaction_queue = append(self$transaction_queue, list(transaction))
                          cat(sprintf("New transcation : %s to %s \n amount : %s",transaction$sender,receiver,amount))

                        },

                        #' @field transaction_queue transaction queue
                        transaction_queue = list(),

                        #' New transaction
                        #' @description
                        #' New transaction
                        #' @param path_private private key
                        #' @param receiver pub key recever
                        #' @param amount amount
                        #' @return data.frame of transactions
                        chain_data = function(){

                          if(base::length(self$blocks)==1){

                            return(data.frame(sender = as.character(), receiver = as.character(), amount = as.numeric()))
                          }

                          data <- self$blocks
                          data = data.frame(do.call(rbind,data[-1]))
                          data = data.frame(do.call(rbind,do.call(rbind,data$data)))
                          data = data %>% mutate(across(everything(),unlist))
                          return(data)
                          invisible(self)
                        },

                        #' account balances
                        #' @description
                        #' account balances
                        #' @return account balances
                        balances = function(){

                          data = self$chain_data() %>% tidyr::pivot_longer(c(receiver,sender)) %>%
                            group_by(value,name) %>% summarise(amount =sum(amount)) %>%
                            mutate(amount = ifelse(name == "sender", 0-amount, amount)) %>%
                            group_by(value) %>% summarise(amount =sum(amount)) %>% setNames(c("id", "balance"))
                          return(data)
                          invisible(self)
                        },


                        #' Print a block
                        #' @description
                        #' Print a block
                        #' @param value block
                        #' @return Print a block
                        print_block = function(value) {
                          value$timestamp = as.character(value$timestamp)
                          cat(paste0(names(value)," : ",value,"\n"))
                        },

                        #' Number of blocks
                        #' @description
                        #' Number of blocks
                        #' @return Number of blocks
                        chain_info = function() {
                          cat(sprintf("Number of blocks : %s \n",private$lengthblock()) )
                        },
                        #' Print chain
                        #' @description
                        #' Print chain
                        #' @return Print chain
                        print_chain = function() {
                          p<-lapply(self$blocks,function(x) {self$print_block(x)
                            cat("\n")})
                        },

                        #' Generate keys
                        #' @description
                        #' Generate a keys
                        #' @param private  private path
                        #' @param public  public public path
                        #' @return Generate key
                        generate_key = function(private,public){

                          key <- rsa_keygen(2048)
                          base64key = base64_encode(key)
                          write_pem(key,private)
                          write_pem(key$pubkey,public)
                          invisible(self)
                        }

                      ),

                      private = list(
                        lengthblock = function() base::length(self$blocks),
                        newBlock = list()
                      )
)
