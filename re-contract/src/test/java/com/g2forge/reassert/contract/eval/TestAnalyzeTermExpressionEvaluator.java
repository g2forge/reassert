package com.g2forge.reassert.contract.eval;

import static com.g2forge.reassert.express.model.operation.BooleanOperation.or;

import org.junit.Test;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.contract.model.contract.TestLicenseTerm;
import com.g2forge.reassert.contract.model.contract.TestNameScheme;
import com.g2forge.reassert.contract.model.contract.TestUsageTerm;
import com.g2forge.reassert.contract.model.finding.ExpressionContextFinding;
import com.g2forge.reassert.contract.model.name.IContractComparisonName;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.model.IExpression;

public class TestAnalyzeTermExpressionEvaluator {
	@Test
	public void test() {
		final IExpression<IContractComparisonName, TermRelation> expression = TestNameScheme.create().rule(b -> b.expression(or(b.notB(TestUsageTerm.Term), b.a(TestLicenseTerm.Permission)))).getExpression();
		final ExpressionContextFinding actual = new AnalyzeTermExpressionEvaluator(TermRelation.Included).eval(expression);
		HAssert.assertEquals(new ExpressionContextFinding(expression, HCollection.asSet(TestUsageTerm.Term, TestLicenseTerm.Permission), HCollection.asSet(TestUsageTerm.Term), null), actual);
	}
}
