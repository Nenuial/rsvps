#* @serializer text
#* @get /calendar/<federation:string>/<year:int>
function(year, federation) {
  rsvps:::pr_fnch_calendar(year, federation)
}


#* @serializer text
#* @get /calendar/<federation:string>
function(federation) {
  rsvps:::pr_fnch_full_calendar(federation)
}

#* @serializer text
#* @get /webcalendar/<federation:string>
function(federation) {
  rsvps:::pr_fnch_web_calendar(federation)
}
