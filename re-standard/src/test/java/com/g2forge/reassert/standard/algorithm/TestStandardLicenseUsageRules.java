package com.g2forge.reassert.standard.algorithm;

import java.util.stream.Collectors;

import org.junit.Test;
import org.slf4j.event.Level;

import com.g2forge.alexandria.java.core.resource.Resource;
import com.g2forge.alexandria.java.function.IPredicate1;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.contract.LicenseUsageAnalyzer;
import com.g2forge.reassert.contract.convert.ReportRenderer;
import com.g2forge.reassert.contract.model.findings.SuspiciousUsageFinding;
import com.g2forge.reassert.contract.model.rules.Rule;
import com.g2forge.reassert.contract.model.rules.Rules;
import com.g2forge.reassert.core.model.contract.ILicenseUsageAnalyzer;
import com.g2forge.reassert.core.model.contract.license.ILicense;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.license.License;
import com.g2forge.reassert.core.model.contract.license.UnspecifiedLicense;
import com.g2forge.reassert.core.model.contract.terms.Terms;
import com.g2forge.reassert.core.model.contract.usage.IUsage;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;
import com.g2forge.reassert.core.model.contract.usage.UnspecifiedUsage;
import com.g2forge.reassert.core.model.contract.usage.Usage;
import com.g2forge.reassert.core.model.report.IFinding;
import com.g2forge.reassert.core.model.report.IReport;
import com.g2forge.reassert.core.model.report.Report;
import com.g2forge.reassert.expression.explain.convert.ExplanationMode;
import com.g2forge.reassert.standard.model.contract.license.StandardLicenseTerm;
import com.g2forge.reassert.standard.model.contract.usage.StandardUsageTerm;

public class TestStandardLicenseUsageRules {
	protected static boolean isSuspicious(IFinding finding) {
		return finding.getInnermostFinding() instanceof SuspiciousUsageFinding;
	}

	@Test
	public void allowed() {
		
		final IUsage usage = Usage.builder().name("Usage").terms(Terms.<IUsageTerm>builder().exclude(StandardUsageTerm.values()).include(StandardUsageTerm.Commercial).build()).build();
		final ILicense license = License.builder().name("License").terms(Terms.<ILicenseTerm>builder().exclude(StandardLicenseTerm.Notice).include(StandardLicenseTerm.CommercialUse).build()).build();
		test(usage, license, Level.INFO, "allowed.txt", IPredicate1.create(TestStandardLicenseUsageRules::isSuspicious).negate());
	}

	@Test
	public void distributionNotice() {
		final IUsage usage = Usage.builder().name("Usage").terms(Terms.<IUsageTerm>builder().exclude(StandardUsageTerm.values()).include(StandardUsageTerm.DistributionPublic).build()).build();
		final ILicense license = License.builder().name("License").terms(Terms.<ILicenseTerm>builder().exclude(StandardLicenseTerm.Notice).include(StandardLicenseTerm.DisclosureSource).include(StandardLicenseTerm.Distribution).build()).build();
		test(usage, license, Level.WARN, "distributionNotice.txt", IPredicate1.create(TestStandardLicenseUsageRules::isSuspicious).negate());
	}

	@Test
	public void invalidRule() {
		final Rule rule = Rule.builder().satisfied(StandardUsageTerm.Commercial).expression(context -> null).build();
		final ILicenseUsageAnalyzer analyzer = new LicenseUsageAnalyzer(Rules.builder().rule(rule).build());;
		HAssert.assertException(IllegalArgumentException.class, "Rule to satisfy \"Commercial usage\" did not use \"Commercial usage\"", () -> analyzer.report(UnspecifiedUsage.create(), UnspecifiedLicense.create()));
	}

	@Test
	public void suspicious() {
		final IUsage usage = Usage.builder().name("Usage").terms(Terms.<IUsageTerm>builder().exclude(StandardUsageTerm.values()).build()).build();
		final ILicense license = UnspecifiedLicense.create();
		test(usage, license, Level.ERROR, "suspicious.txt", TestStandardLicenseUsageRules::isSuspicious);
	}

	protected void test(final IUsage usage, final ILicense license, final Level exepctedMinLevel, final String expectedResource, IPredicate1<IFinding> filter) {
		final IReport reportRaw = new LicenseUsageAnalyzer(StandardLicenseUsageRules.create()).report(usage, license);
		final IReport reportClean = filter != null ? Report.builder().findings(reportRaw.getFindings().stream().filter(filter).collect(Collectors.toList())).build() : reportRaw;

		final ReportRenderer reportRenderer = new ReportRenderer(ExplanationMode.Explain);

		HAssert.assertTrue(reportRenderer.render(reportRaw), reportClean.getMinLevel().compareTo(exepctedMinLevel) >= 0);
		final String rendered = reportRenderer.render(Report.builder().findings(reportClean.getFindings().stream().filter(finding -> finding.getLevel().compareTo(Level.WARN) <= 0).collect(Collectors.toList())).build());
		HAssert.assertEquals(new Resource(getClass(), expectedResource), rendered);
	}

	@Test
	public void unspecified() {
		final IUsage usage = Usage.builder().name("Usage").terms(Terms.<IUsageTerm>builder().exclude(StandardUsageTerm.values()).include(StandardUsageTerm.Commercial).build()).build();
		final ILicense license = UnspecifiedLicense.create();
		test(usage, license, Level.ERROR, "unspecified.txt", IPredicate1.create(TestStandardLicenseUsageRules::isSuspicious).negate());
	}
}
