package com.g2forge.reassert.contract;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.contract.TermRelationBooleanSystem;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.evaluate.ATestExplainedBooleanEvaluator;
import com.g2forge.reassert.express.evaluate.bool.IBooleanSystem;
import com.g2forge.reassert.express.express.Literal;
import com.g2forge.reassert.express.express.Operation;

public class TestTermRelationBooleanSystem extends ATestExplainedBooleanEvaluator<TermRelation> {
	@Test
	public void and() {
		HAssert.assertEquals(TermRelation.Unspecified, evaluate(Operation.Operator.AND.<TermRelation>builder().argument$(TermRelation.Unspecified).argument$(TermRelation.Unspecified).build()).get());
		HAssert.assertEquals(TermRelation.Excluded, evaluate(Operation.Operator.AND.<TermRelation>builder().argument$(TermRelation.Excluded).argument$(TermRelation.Unspecified).build()).get());
		HAssert.assertEquals(TermRelation.Unspecified, evaluate(Operation.Operator.AND.<TermRelation>builder().argument$(TermRelation.Included).argument$(TermRelation.Unspecified).build()).get());

		HAssert.assertEquals(TermRelation.Excluded, evaluate(Operation.Operator.AND.<TermRelation>builder().argument$(TermRelation.Excluded).argument$(TermRelation.Excluded).build()).get());
		HAssert.assertEquals(TermRelation.Excluded, evaluate(Operation.Operator.AND.<TermRelation>builder().argument$(TermRelation.Included).argument$(TermRelation.Excluded).build()).get());

		HAssert.assertEquals(TermRelation.Included, evaluate(Operation.Operator.AND.<TermRelation>builder().argument$(TermRelation.Included).argument$(TermRelation.Included).build()).get());
	}

	@Test
	public void andExplanation() {
		HAssert.assertEquals("Excluded (&&) - because Excluded is the zero of the && operator\n\t* Excluded\n\t_ Unspecified", explain(Operation.Operator.AND.<TermRelation>builder().argument$(TermRelation.Excluded).argument$(TermRelation.Unspecified).build()));
	}

	@Override
	protected IBooleanSystem<TermRelation> createSystem() {
		return TermRelationBooleanSystem.create();
	}

	@Test
	public void literal() {
		HAssert.assertEquals("Unspecified - Input", explain(new Literal<>("Input", TermRelation.Unspecified)));
	}

	@Test
	public void or() {
		HAssert.assertEquals(TermRelation.Unspecified, evaluate(Operation.Operator.OR.<TermRelation>builder().argument$(TermRelation.Unspecified).argument$(TermRelation.Unspecified).build()).get());
		HAssert.assertEquals(TermRelation.Unspecified, evaluate(Operation.Operator.OR.<TermRelation>builder().argument$(TermRelation.Excluded).argument$(TermRelation.Unspecified).build()).get());
		HAssert.assertEquals(TermRelation.Included, evaluate(Operation.Operator.OR.<TermRelation>builder().argument$(TermRelation.Included).argument$(TermRelation.Unspecified).build()).get());

		HAssert.assertEquals(TermRelation.Excluded, evaluate(Operation.Operator.OR.<TermRelation>builder().argument$(TermRelation.Excluded).argument$(TermRelation.Excluded).build()).get());
		HAssert.assertEquals(TermRelation.Included, evaluate(Operation.Operator.OR.<TermRelation>builder().argument$(TermRelation.Included).argument$(TermRelation.Excluded).build()).get());

		HAssert.assertEquals(TermRelation.Included, evaluate(Operation.Operator.OR.<TermRelation>builder().argument$(TermRelation.Included).argument$(TermRelation.Included).build()).get());
	}
}
