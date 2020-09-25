package com.g2forge.reassert.contract;

import org.junit.Test;
import org.slf4j.event.Level;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.LicenseUsageNameScheme;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.finding.ConditionFinding;
import com.g2forge.reassert.contract.model.contract.TestLicenseTerm;
import com.g2forge.reassert.contract.model.contract.TestUsageTerm;
import com.g2forge.reassert.contract.model.rule.ContractComparisonRules;
import com.g2forge.reassert.core.model.contract.license.GeneralLicense;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.terms.Terms;
import com.g2forge.reassert.core.model.contract.usage.GeneralUsage;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;
import com.g2forge.reassert.core.model.report.IReport;
import com.g2forge.reassert.core.model.report.Report;
import static com.g2forge.reassert.express.model.operation.BooleanOperation.or;

public class TestContractComparisonAnalyzer {
	protected IReport analyze(final Terms<IUsageTerm> usageTerms, final Terms<ILicenseTerm> licenseTerms) {
		final ContractComparisonRules rules = new ContractComparisonRules(LicenseUsageNameScheme.create().rule(b -> b.expression(or(b.notB(TestUsageTerm.Term), b.a(TestLicenseTerm.Permission))).finding(ConditionFinding::new)));
		final GeneralUsage usage = new GeneralUsage("usage", "usage", usageTerms);
		final GeneralLicense license = new GeneralLicense("license", "license", "license", licenseTerms, null, false);

		final Report.ReportBuilder builder = Report.builder();
		new ContractComparisonAnalyzer(rules).analyze(license, usage, builder);
		return builder.build();
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
	public void ignore() {
		final ContractComparisonRules rules = new ContractComparisonRules(LicenseUsageNameScheme.create().rule(b -> b.expression(b.a(TestLicenseTerm.Condition))));

		final GeneralUsage usage = new GeneralUsage("usage", "usage", Terms.<IUsageTerm>builder().build());
		final GeneralLicense license = new GeneralLicense("license", "license", "license", Terms.<ILicenseTerm>builder().build(), null, false);

		final Report.ReportBuilder builder = Report.builder();
		new ContractComparisonAnalyzer(rules).analyze(license, usage, builder);
		final Report report = builder.build();

		final String message = report.getFindings().toString();
		HAssert.assertNull(message, report.getMinLevel());
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
