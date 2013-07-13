/*
 * YARR - yet another rss reader
 *
 * Copyright (C) 2013  Semjon Popugaev (senia).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.antabaka.yarr

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class DownloaderSpec extends FunSpec with ShouldMatchers with FailMatching {
  describe("A Downloader") {
    it ("should load rss from test host") {
      Downloader.getRawData("http://habrahabr.ru/rss/hubs/all/").isSuccess should equal(true)
    }

    it ("should load and parse rss from test host") {
      Downloader.getRssPacket("http://habrahabr.ru/rss/hubs/all/", "EEE, dd MMM yyyy HH:mm:ss zzz").isSuccess should equal(true)
    }
  }
}
