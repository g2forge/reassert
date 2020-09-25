package com.g2forge.reassert.contract.convert;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.finding.ConditionFinding;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.finding.CopyrightNoticeFinding;
import com.g2forge.reassert.contract.convert.ReportRenderer;
import com.g2forge.reassert.core.api.module.Context;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.core.model.report.Report;
import com.g2forge.reassert.express.convert.ExplanationMode;
import com.g2forge.reassert.express.model.constant.Literal;

public class TestReportRenderer {
	@Test
	public void empty() {
		HAssert.assertEquals("Minimum finding level: NONE\n", new ReportRenderer(ExplanationMode.Explain, Context.getContext()).render(Report.builder().build()));
	}

	@Test
	public void fail() {
		HAssert.assertEquals("Minimum finding level: ERROR\nERROR: Condition is not satisfied\n\tExplanation: Excluded\n", new ReportRenderer(ExplanationMode.Explain, Context.getContext()).render(Report.builder().finding(new ConditionFinding(new Literal<>(TermRelation.Excluded))).build()));
	}

	@Test
	public void pass() {
		HAssert.assertEquals("Minimum finding level: INFO\nINFO: You must publish a copyright and license notice stating that you use this artifact\n\tExplanation: Excluded\n", new ReportRenderer(ExplanationMode.Explain, Context.getContext()).render(Report.builder().finding(new CopyrightNoticeFinding(new Literal<>(TermRelation.Excluded))).build()));
	}
}
