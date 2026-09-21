#* @get /echo
function(msg = "") {
  list(msg = paste0("The message is: '", msg, "'"))
}
