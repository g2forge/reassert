package com.g2forge.reassert.standard.model.contract.license;

import java.util.List;

import org.junit.runners.Parameterized.Parameters;

import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

public class TestBSDStandardLicenseParser extends ATestStandardLicenseParser {
	@Data
	@Builder(toBuilder = true)
	@RequiredArgsConstructor
	public static class TestCase implements ITestCase {
		protected final int clauses;

		protected final String text;

		protected final String purpose;

		@Override
		public ILicenseApplied getLicenseApplied() {
			return new BSDLicense(getClauses());
		}
	}

	@Parameters(name = "{1}")
	public static List<Object[]> computeTestParameters() {
		return ATestStandardLicenseParser.computeTestParameters(TestCase.class, "bsd license texts.csv");
	}
}
