import language.experimental.namedTuples // experimental as of Scala 3.6
import typology.Tuplelogy.mapByName

@main
def example =

  type Envelope = (
      sender: String,
      recipient: String,
      subject: String
  )

  val message = (
    sender    = "alfred@tuplelogy.org", 
    recipient = "gregg@example.com",
    subject   = "Check this out!",
    body      = "[...]"
  )

  def validateEnvelope(env: Envelope) = 
    println(env)

  validateEnvelope(message.mapByName) // works!
