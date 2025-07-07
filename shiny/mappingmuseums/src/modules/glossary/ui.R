glossaryUI <- function(id) {
  fluidPage(
    text_box("GLOSSARY-TOP"),

    h3("Museum Attributes"),
    p("In the visualizations, museums can be grouped according to their attributes: size, governance, subject matter, and accreditation."),
    h4("Size"),
    p(""),
    h4("Governance"),
    p(""),
    img(src='governance_types.png', width="80%"),
    h4("Subject Matter"),
    p(""),
    h4("Accreditation"),
    p(""),

    h3("Actor Types"),
    p("The types of actor involved in transactions with museums are shown below. The leaves of the tree include highly specific categories. These are grouped into core categories which are used in some of the visual summaries in the following tabs."),
    img(src='actor_types.png', width="80%"),

    h3("Event Types"),
    p("The types of event involving museum collections are shown below."),
    p("Events can be changes of ownership, changes of custody, both, or neither. Alternatively they can mark the end of a collection's or object's existence. The taxonomy colours each event type according to the default way in which it should be understood. Individual events can override this default. For example, some sales do not result in a change of custody."),
    img(src='event_types.png', width="80%"),
    DTOutput(NS(id, "eventTypesGlossary")),

    h3("Types of Reason for Closure"),
    p("The types of reason for museum closure are shown below."),
    img(src='reason_types.png', width="80%")

  )
}
