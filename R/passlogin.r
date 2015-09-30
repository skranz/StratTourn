passwordLogin = function(id="",password,login.fun, title="", text="<p>Enter password to login.<p>", failure.msg="<p>You entered a wrong password. Please try again.</p>", success.msg = "<p>Correct password. Logging in...</p>", sleep.secs=0.5) {

  passId = paste0(id,"___loginPasswordInput")
  btnId = paste0(id,"___loginPasswordBtn")
  msgId  = paste0(id,"___loginPasswordMsg")
  ui = list(
    h4(title),
    HTML(text),
    passwordInput(passId,"Password:"),
    actionButton(btnId, "Login"),
    uiOutput(msgId)
  )
  buttonHandler(btnId,passId=passId,password=password, function(app,passId,...) {
    restore.point("passwordButtonHandler")
    
    entered = getInputValue(passId)
    if (identical(password, entered)) {
      setUI(msgId, HTML(success.msg))
      login.fun()
    } else {
      
      setUI(msgId, HTML(failure.msg))
    }
  })
  ui
}