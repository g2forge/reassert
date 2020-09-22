package com.g2forge.reassert.contract.v2.algorithm;

import static com.g2forge.reassert.contract.v2.model.TermOperation.not;
import static com.g2forge.reassert.contract.v2.model.TermOperation.of;
import static com.g2forge.reassert.contract.v2.model.TermOperation.or;

import org.junit.Test;
import org.slf4j.event.Level;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.contract.v2.model.contract.TestLicenseTerm;
import com.g2forge.reassert.contract.v2.model.contract.TestUsageTerm;
import com.g2forge.reassert.contract.v2.model.finding.rule.ConditionFinding;
import com.g2forge.reassert.contract.v2.model.rule.Rule;
import com.g2forge.reassert.contract.v2.model.rule.Rules;
import com.g2forge.reassert.core.model.contract.license.GeneralLicense;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.terms.Terms;
import com.g2forge.reassert.core.model.contract.usage.GeneralUsage;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;
import com.g2forge.reassert.core.model.report.IReport;

public class TestLicenseUsageAnalyzer {
	protected IReport analyze(final Terms<IUsageTerm> usageTerms, final Terms<ILicenseTerm> licenseTerms) {
		final Rules rules = new Rules(Rule.builder().expression(or(not(TestUsageTerm.Term), of(TestLicenseTerm.Permission))).finding(ConditionFinding::new).build());
		final LicenseUsageAnalyzer analyzer = new LicenseUsageAnalyzer(rules);

		final GeneralUsage usage = new GeneralUsage("usage", "usage", usageTerms);
		final GeneralLicense license = new GeneralLicense("license", "license", "license", licenseTerms, null, false);
		final IReport report = analyzer.report(usage, license);
		return report;
	}

	@Test
	public void fail() {
		final IReport report = analyze(Terms.<IUsageTerm>builder().include(TestUsageTerm.Term).build(), Terms.<ILicenseTerm>builder().build());
		final String message = report.getFindings().toString();
		HAssert.assertTrue(message, report.getMinLevel().compareTo(Level.ERROR) <= 0);
		HAssert.assertEquals(message, 1, report.getFindings().size());
		HAssert.assertInstanceOf(ConditionFinding.class, HCollection.getOne(report.getFindings()).getInnermostFinding());

	}

	@Test
	public void pass() {
		final IReport report = analyze(Terms.<IUsageTerm>builder().include(TestUsageTerm.Term).build(), Terms.<ILicenseTerm>builder().include(TestLicenseTerm.Permission).build());
		final String message = report.getFindings().toString();
		HAssert.assertTrue(message, report.getMinLevel().compareTo(Level.INFO) >= 0);
		HAssert.assertEquals(message, 1, report.getFindings().size());
		HAssert.assertInstanceOf(ConditionFinding.class, HCollection.getOne(report.getFindings()).getInnermostFinding());
	}
}
