package com.g2forge.reassert.standard.algorithm;

import java.util.stream.Collectors;

import org.junit.Test;
import org.slf4j.event.Level;

import com.g2forge.alexandria.java.core.resource.Resource;
import com.g2forge.alexandria.java.function.IPredicate1;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.contract.algorithm.licenseusage.LicenseUsageAnalyzer;
import com.g2forge.reassert.contract.convert.ReportRenderer;
import com.g2forge.reassert.contract.model.finding.rule.SuspiciousUsageFinding;
import com.g2forge.reassert.core.api.module.Context;
import com.g2forge.reassert.core.model.contract.license.GeneralLicense;
import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.license.UnspecifiedLicense;
import com.g2forge.reassert.core.model.contract.terms.Terms;
import com.g2forge.reassert.core.model.contract.usage.GeneralUsage;
import com.g2forge.reassert.core.model.contract.usage.IUsageApplied;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;
import com.g2forge.reassert.core.model.report.IFinding;
import com.g2forge.reassert.core.model.report.IReport;
import com.g2forge.reassert.core.model.report.Report;
import com.g2forge.reassert.express.convert.ExplanationMode;
import com.g2forge.reassert.standard.algorithm.StandardLicenseUsageRules;
import com.g2forge.reassert.standard.model.contract.license.StandardLicenseTerm;
import com.g2forge.reassert.standard.model.contract.usage.StandardUsageTerm;

public class TestStandardLicenseUsageRules {
	protected static boolean isSuspicious(IFinding finding) {
		return finding.getInnermostFinding() instanceof SuspiciousUsageFinding;
	}

	@Test
	public void allowed() {
		final IUsageApplied usage = GeneralUsage.builder().name("Usage").terms(Terms.<IUsageTerm>builder().exclude(StandardUsageTerm.values()).include(StandardUsageTerm.Commercial).build()).build();
		final ILicenseApplied license = GeneralLicense.builder().name("License").terms(Terms.<ILicenseTerm>builder().exclude(StandardLicenseTerm.Notice).include(StandardLicenseTerm.CommercialUse).build()).build();
		test(usage, license, Level.INFO, "allowed.txt", IPredicate1.create(TestStandardLicenseUsageRules::isSuspicious).negate());
	}

	@Test
	public void distributionNotice() {
		final IUsageApplied usage = GeneralUsage.builder().name("Usage").terms(Terms.<IUsageTerm>builder().exclude(StandardUsageTerm.values()).include(StandardUsageTerm.DistributionPublic).build()).build();
		final ILicenseApplied license = GeneralLicense.builder().name("License").terms(Terms.<ILicenseTerm>builder().exclude(StandardLicenseTerm.Notice).include(StandardLicenseTerm.DisclosureSource).include(StandardLicenseTerm.Distribution).build()).build();
		test(usage, license, Level.WARN, "distributionNotice.txt", IPredicate1.create(TestStandardLicenseUsageRules::isSuspicious).negate());
	}

	@Test
	public void suspicious() {
		final IUsageApplied usage = GeneralUsage.builder().name("Usage").terms(Terms.<IUsageTerm>builder().exclude(StandardUsageTerm.values()).build()).build();
		final ILicenseApplied license = UnspecifiedLicense.create();
		test(usage, license, Level.ERROR, "suspicious.txt", TestStandardLicenseUsageRules::isSuspicious);
	}

	protected void test(final IUsageApplied usage, final ILicenseApplied license, final Level exepctedMinLevel, final String expectedResource, IPredicate1<IFinding> filter) {
		final IReport reportRaw = new LicenseUsageAnalyzer(StandardLicenseUsageRules.create()).report(usage, license);
		final IReport reportClean = filter != null ? Report.builder().findings(reportRaw.getFindings().stream().filter(filter).collect(Collectors.toList())).build() : reportRaw;

		final ReportRenderer reportRenderer = new ReportRenderer(ExplanationMode.Explain, Context.getContext());

		HAssert.assertTrue(reportRenderer.render(reportRaw), reportClean.getMinLevel().compareTo(exepctedMinLevel) >= 0);
		final String rendered = reportRenderer.render(Report.builder().findings(reportClean.getFindings().stream().filter(finding -> finding.getLevel().compareTo(Level.WARN) <= 0).collect(Collectors.toList())).build());
		HAssert.assertEquals(new Resource(getClass(), expectedResource), rendered);
	}

	@Test
	public void unspecified() {
		final IUsageApplied usage = GeneralUsage.builder().name("Usage").terms(Terms.<IUsageTerm>builder().exclude(StandardUsageTerm.values()).include(StandardUsageTerm.Commercial).build()).build();
		final ILicenseApplied license = UnspecifiedLicense.create();
		test(usage, license, Level.ERROR, "unspecified.txt", IPredicate1.create(TestStandardLicenseUsageRules::isSuspicious).negate());
	}
}
