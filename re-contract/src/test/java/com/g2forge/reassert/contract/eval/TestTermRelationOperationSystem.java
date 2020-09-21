package com.g2forge.reassert.contract.eval;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.v2.convert.ExplanationMode;
import com.g2forge.reassert.express.v2.convert.ExplanationRenderer;
import com.g2forge.reassert.express.v2.eval.ExplainingEvaluator;
import com.g2forge.reassert.express.v2.eval.ValueEvaluator;
import com.g2forge.reassert.express.v2.model.IExplained;
import com.g2forge.reassert.express.v2.model.IExpression;
import com.g2forge.reassert.express.v2.model.constant.Literal;
import com.g2forge.reassert.express.v2.model.operation.BooleanOperation;

import lombok.Getter;

public class TestTermRelationOperationSystem {
	@Getter(lazy = true)
	private static final ValueEvaluator<String, TermRelation> evaluator = new ValueEvaluator<String, TermRelation>(TermRelationValueSystem.create(), TermRelationOperationSystem.create());

	@Test
	public void and() {
		HAssert.assertEquals(TermRelation.Unspecified, evaluate(BooleanOperation.Operator.AND.<String, TermRelation>builder().argument$(TermRelation.Unspecified).argument$(TermRelation.Unspecified).build()));
		HAssert.assertEquals(TermRelation.Excluded, evaluate(BooleanOperation.Operator.AND.<String, TermRelation>builder().argument$(TermRelation.Excluded).argument$(TermRelation.Unspecified).build()));
		HAssert.assertEquals(TermRelation.Unspecified, evaluate(BooleanOperation.Operator.AND.<String, TermRelation>builder().argument$(TermRelation.Included).argument$(TermRelation.Unspecified).build()));

		HAssert.assertEquals(TermRelation.Excluded, evaluate(BooleanOperation.Operator.AND.<String, TermRelation>builder().argument$(TermRelation.Excluded).argument$(TermRelation.Excluded).build()));
		HAssert.assertEquals(TermRelation.Excluded, evaluate(BooleanOperation.Operator.AND.<String, TermRelation>builder().argument$(TermRelation.Included).argument$(TermRelation.Excluded).build()));

		HAssert.assertEquals(TermRelation.Included, evaluate(BooleanOperation.Operator.AND.<String, TermRelation>builder().argument$(TermRelation.Included).argument$(TermRelation.Included).build()));
	}

	@Test
	public void andExplanation() {
		HAssert.assertEquals("Excluded - because anything and-ed with Excluded is Excluded\n\t> Excluded", explain(BooleanOperation.Operator.AND.<String, TermRelation>builder().argument$(TermRelation.Excluded).argument$(TermRelation.Unspecified).build()));
	}

	protected TermRelation evaluate(IExpression<String, TermRelation> expression) {
		return getEvaluator().eval(expression);
	}

	protected String explain(IExpression<String, TermRelation> expression) {
		final ExplainingEvaluator<String, TermRelation> evaluator = new ExplainingEvaluator<>(TermRelationValueSystem.create(), TermRelationOperationSystem.create());
		final ExplanationRenderer<String, TermRelation> renderer = new ExplanationRenderer<>(ExplanationMode.Explain, TermRelationValueSystem.create(), TermRelationOperationSystem.create());
		final IExplained<TermRelation> explained = evaluator.eval(expression);
		return renderer.render(explained);
	}

	@Test
	public void literal() {
		HAssert.assertEquals("Unspecified - Input", explain(new Literal<>("Input", TermRelation.Unspecified)));
	}

	@Test
	public void or() {
		HAssert.assertEquals(TermRelation.Unspecified, evaluate(BooleanOperation.Operator.OR.<String, TermRelation>builder().argument$(TermRelation.Unspecified).argument$(TermRelation.Unspecified).build()));
		HAssert.assertEquals(TermRelation.Unspecified, evaluate(BooleanOperation.Operator.OR.<String, TermRelation>builder().argument$(TermRelation.Excluded).argument$(TermRelation.Unspecified).build()));
		HAssert.assertEquals(TermRelation.Included, evaluate(BooleanOperation.Operator.OR.<String, TermRelation>builder().argument$(TermRelation.Included).argument$(TermRelation.Unspecified).build()));

		HAssert.assertEquals(TermRelation.Excluded, evaluate(BooleanOperation.Operator.OR.<String, TermRelation>builder().argument$(TermRelation.Excluded).argument$(TermRelation.Excluded).build()));
		HAssert.assertEquals(TermRelation.Included, evaluate(BooleanOperation.Operator.OR.<String, TermRelation>builder().argument$(TermRelation.Included).argument$(TermRelation.Excluded).build()));

		HAssert.assertEquals(TermRelation.Included, evaluate(BooleanOperation.Operator.OR.<String, TermRelation>builder().argument$(TermRelation.Included).argument$(TermRelation.Included).build()));
	}

	@Test
	public void xor() {
		HAssert.assertEquals(TermRelation.Unspecified, evaluate(BooleanOperation.Operator.XOR.<String, TermRelation>builder().argument$(TermRelation.Unspecified).argument$(TermRelation.Unspecified).build()));
		HAssert.assertEquals(TermRelation.Unspecified, evaluate(BooleanOperation.Operator.XOR.<String, TermRelation>builder().argument$(TermRelation.Excluded).argument$(TermRelation.Unspecified).build()));
		HAssert.assertEquals(TermRelation.Unspecified, evaluate(BooleanOperation.Operator.XOR.<String, TermRelation>builder().argument$(TermRelation.Included).argument$(TermRelation.Unspecified).build()));

		HAssert.assertEquals(TermRelation.Excluded, evaluate(BooleanOperation.Operator.XOR.<String, TermRelation>builder().argument$(TermRelation.Excluded).argument$(TermRelation.Excluded).build()));
		HAssert.assertEquals(TermRelation.Included, evaluate(BooleanOperation.Operator.XOR.<String, TermRelation>builder().argument$(TermRelation.Included).argument$(TermRelation.Excluded).build()));

		HAssert.assertEquals(TermRelation.Excluded, evaluate(BooleanOperation.Operator.XOR.<String, TermRelation>builder().argument$(TermRelation.Included).argument$(TermRelation.Included).build()));
	}
}
