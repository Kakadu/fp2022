def send_request(url, on_succ, on_fail)
    if url == "google.com" then
        on_succ ("123 results")
    else
        on_fail ("Unknown resource")
    end
end

on_success = lambda {|msg| "Success! Message: " + msg}
on_failure = lambda {|msg| "Connection failed.. Message: " + msg }

send_request ("yahoo.com", on_success, on_failure)