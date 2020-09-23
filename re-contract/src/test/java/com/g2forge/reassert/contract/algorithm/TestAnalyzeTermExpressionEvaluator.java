package com.g2forge.reassert.contract.algorithm;

import static com.g2forge.reassert.contract.model.licenseusage.CTOperation.not;
import static com.g2forge.reassert.contract.model.licenseusage.CTOperation.of;
import static com.g2forge.reassert.contract.model.licenseusage.CTOperation.or;

import org.junit.Test;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.contract.algorithm.AnalyzeTermExpressionEvaluator;
import com.g2forge.reassert.contract.model.contract.TestLicenseTerm;
import com.g2forge.reassert.contract.model.contract.TestUsageTerm;
import com.g2forge.reassert.contract.model.finding.ExpressionContextFinding;
import com.g2forge.reassert.contract.model.licenseusage.CTOperation;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;

public class TestAnalyzeTermExpressionEvaluator {
	@Test
	public void test() {
		final CTOperation expression = or(not(TestUsageTerm.Term), of(TestLicenseTerm.Permission));
		final ExpressionContextFinding actual = new AnalyzeTermExpressionEvaluator(TermRelation.Included).eval(expression);
		HAssert.assertEquals(new ExpressionContextFinding(expression, HCollection.asSet(TestUsageTerm.Term, TestLicenseTerm.Permission), HCollection.asSet(TestUsageTerm.Term), null), actual);
	}
}
