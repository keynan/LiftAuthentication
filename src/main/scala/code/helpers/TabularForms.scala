package ca.dualityStudios.liftAuthentication.helpers

import net.liftweb.util._
import scala.xml.{NodeSeq, Elem}

trait TabularForms {
	
	import BindHelpers._

	def form(method: String, snippet: String) = 
		<form method={method}>
			<table> 
				<tr class={"lift:" + snippet}>
					<td class="lbl"></td>
					<td><input id="inp" /></td>
				</tr>
			</table>
		</form>
	
	def applyTemplate(template: NodeSeq)(field: FormField) = {
		(
			".lbl *" #> field.label &
			"#inp" #> field.input()
		).apply(template)
	}
	
	def fieldRender(fields: List[FormField])(template: NodeSeq): NodeSeq = 
		fields.flatMap(applyTemplate(template))
}

case class FormField(val label: String, val input:() => Elem)
