package com.g2forge.reassert.standard.model.contract.license;

import java.util.List;

import org.junit.runners.Parameterized.Parameters;

import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.license.LicenseVersion;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

public class TestFamilyVersionStandardLicenseParser extends ATestStandardLicenseParser {
	@Data
	@Builder(toBuilder = true)
	@RequiredArgsConstructor
	public static class TestCase implements ITestCase {
		protected final StandardLicenseFamily family;

		protected final String version;

		protected final boolean orLater;

		protected final String text;

		protected final String purpose;

		@Override
		public ILicenseApplied getLicenseApplied() {
			final String versionString = getVersion();
			final LicenseVersion version = ((versionString == null) || (versionString.length() == 0)) ? null : new LicenseVersion(versionString);

			return getFamily().create(version, isOrLater());
		}
	}

	@Parameters(name = "{1}")
	public static List<Object[]> computeTestParameters() {
		return ATestStandardLicenseParser.computeTestParameters(TestCase.class, "familyversion license texts.csv");
	}
}
