package models.template

import views.html.helper.FieldConstructor

object InlineInput {
  implicit val inlineInput = FieldConstructor(views.html.template.inlineInput.render)
}
